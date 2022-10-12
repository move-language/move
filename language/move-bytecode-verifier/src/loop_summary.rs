// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

use move_binary_format::control_flow_graph::{BlockId, ControlFlowGraph, VMControlFlowGraph};
use std::collections::{btree_map::Entry, BTreeMap, BTreeSet};

/// Dense index into nodes in the same `LoopSummary`
#[derive(Copy, Clone, Debug, PartialEq, Eq, PartialOrd, Ord)]
pub struct NodeId(u16);

/// Summarise loop information from a control-flow graph by calculating its depth-first spanning
/// tree (DFST) and then using that to:
///
/// - Distinguish edges in the CFG as:
///   - tree edges (in the spanning tree)
///   - back edges (going backward from descendant to ancestor in the spanning tree)
///   - ... everything else.
/// - Calculate which node is descendant of which in the DFST
/// - Enable collapsing nodes that form loops down into a single representative node which remembers
///   its loop nesting depth (how many levels of nesting are contained within it).
/// - Mapping nodes in the summary back to the blocks they originate from in the CFG.
///
/// This functionality is used to implement Tarjan's Loop Reducibility algorithm (Tarjan 1974).
pub struct LoopSummary {
    /// Original block corresponding to this loop, useful for recovering code offsets, e.g. for
    /// error messages.
    blocks: Vec<BlockId>,

    /// Number of transitive descendants for a loop, in the depth-first spanning tree.
    descs: Vec<u16>,

    /// The incoming edges for a loop are partitioned between `back_edges` which create cycles in
    /// the DFS tree, and `pred_edges` which are all the rest.
    backs: Vec<Vec<NodeId>>,
    preds: Vec<Vec<NodeId>>,

    /// A disjoint-set data structure used when collapsing loops down to single nodes in the summary
    /// graph.
    parents: Vec<NodeId>,

    /// The nesting depth of (collapsed) loops in the summary graph.  Initially all loops are
    /// uncollapsed with depth 0.
    depths: Vec<u16>,
}

impl LoopSummary {
    pub fn new(cfg: &VMControlFlowGraph) -> Self {
        use Exploration::*;
        use Frontier::*;

        enum Exploration {
            InProgress(NodeId),
            Done(NodeId),
        }

        enum Frontier {
            Visit {
                from_node: NodeId,
                to_block: BlockId,
            },
            Finish {
                block: BlockId,
                node_id: NodeId,
                parent: NodeId,
            },
        }

        let num_blocks = cfg.num_blocks() as usize;

        // Fields in LoopSummary that are filled via a depth-first traversal of `cfg`.
        let mut blocks = vec![0; num_blocks];
        let mut descs = vec![0; num_blocks];
        let mut backs = vec![vec![]; num_blocks];
        let mut preds = vec![vec![]; num_blocks];

        let mut next_node = NodeId(0);

        let root_block = cfg.entry_block_id();
        let root_node = next_node.bump();

        let mut exploration = BTreeMap::new();
        blocks[usize::from(root_node)] = root_block;
        exploration.insert(root_block, InProgress(root_node));

        let mut stack: Vec<Frontier> = cfg
            .successors(root_block)
            .iter()
            .map(|succ| Visit {
                from_node: root_node,
                to_block: *succ,
            })
            .collect();

        while let Some(action) = stack.pop() {
            match action {
                Finish {
                    block,
                    node_id,
                    parent,
                } => {
                    descs[usize::from(parent)] += 1 + descs[usize::from(node_id)];
                    *exploration.get_mut(&block).unwrap() = Done(node_id);
                }

                Visit {
                    from_node,
                    to_block,
                } => match exploration.entry(to_block) {
                    Entry::Occupied(entry) => match entry.get() {
                        // Cyclic back edge detected by re-visiting `to` while still processing its
                        // children.
                        InProgress(to_node) => backs[usize::from(*to_node)].push(from_node),

                        // Cross edge detected by re-visiting `to` after finishing processing its
                        // children.
                        Done(to_node) => preds[usize::from(*to_node)].push(from_node),
                    },

                    // Visiting `to` for the first time: `from` must be its parent in the depth-
                    // -first spanning tree, and we should continue exploring its successors.
                    Entry::Vacant(entry) => {
                        let to_node = next_node.bump();
                        entry.insert(InProgress(to_node));
                        blocks[usize::from(to_node)] = to_block;
                        preds[usize::from(to_node)].push(from_node);

                        stack.push(Finish {
                            block: to_block,
                            node_id: to_node,
                            parent: from_node,
                        });

                        stack.extend(cfg.successors(to_block).iter().map(|succ| Visit {
                            from_node: to_node,
                            to_block: *succ,
                        }));
                    }
                },
            }
        }

        LoopSummary {
            blocks,
            descs,
            backs,
            preds,
            parents: (0..num_blocks).map(|id| NodeId(id as u16)).collect(),
            depths: vec![0; num_blocks],
        }
    }

    /// Decides whether `descendant` is an descendant of `ancestor` in the depth-first spanning
    /// tree.
    pub fn is_descendant(&self, NodeId(ancestor): NodeId, NodeId(descendant): NodeId) -> bool {
        // All the descendants of `ancestor` in the DFST will have the IDs immediately following
        // it.
        ancestor <= descendant && descendant <= ancestor + self.descs[ancestor as usize]
    }

    /// Find the head of the collapsed node containing loop `id`, use path-compression to speed up
    /// future accesses.
    pub fn containing_loop(&mut self, id: NodeId) -> NodeId {
        let mut child = id;
        let mut parent = self.parent(child);
        let mut grandparent = self.parent(parent);

        if child == parent || parent == grandparent {
            return parent;
        }

        let mut descendants = vec![];
        loop {
            // Invariant: child -> parent -> grandparent
            //       and  parent != grandparent
            //       and  forall d in descendants. parent(d) != parent(parent(d))
            descendants.push(child);
            (child, parent, grandparent) = (parent, grandparent, self.parent(grandparent));
            if parent == grandparent {
                break;
            }
        }

        for descendant in descendants {
            *self.parent_mut(descendant) = parent;
        }

        parent
    }

    /// Collapse `body` of a loop down to one node, represented by its `head`.  Calculate the
    /// nesting depth of the collapsed node and return it.
    ///
    /// Assumes that all the loops involved are the heads of their corresponding nodes in the
    /// summary graph.
    ///
    /// Note that this function can be called with an empty body, meaning `head` is the only
    /// node in the loop, which implies it will be nested more deeply.
    pub fn collapse_loop(&mut self, head: NodeId, body: &BTreeSet<NodeId>) -> u16 {
        debug_assert_eq!(head, self.parent(head));

        let mut depth = self.depth(head);
        for constituent in body {
            debug_assert_eq!(*constituent, self.parent(*constituent));
            *self.parent_mut(*constituent) = head;
            depth = self.depth(*constituent).max(depth);
        }

        depth += 1;
        *self.depth_mut(head) = depth;
        depth
    }

    /// Returns an iterator over `NodeId`s in this `LoopSummary` in pre-order according to its
    /// depth-first spanning tree.
    pub fn preorder(&self) -> impl DoubleEndedIterator<Item = NodeId> {
        // `LoopSummary::new` assigns `NodeId`s to blocks in preorder, so just return the natural
        // order.
        (0..self.blocks.len()).map(|id| NodeId(id as u16))
    }

    /// Per-loop accessors
    pub fn block(&self, l: NodeId) -> BlockId {
        self.blocks[usize::from(l)]
    }

    pub fn back_edges(&self, l: NodeId) -> &Vec<NodeId> {
        &self.backs[usize::from(l)]
    }

    pub fn pred_edges(&self, l: NodeId) -> &Vec<NodeId> {
        &self.preds[usize::from(l)]
    }

    fn parent(&self, l: NodeId) -> NodeId {
        self.parents[usize::from(l)]
    }

    fn parent_mut(&mut self, l: NodeId) -> &mut NodeId {
        &mut self.parents[usize::from(l)]
    }

    fn depth(&self, l: NodeId) -> u16 {
        self.depths[usize::from(l)]
    }

    fn depth_mut(&mut self, l: NodeId) -> &mut u16 {
        &mut self.depths[usize::from(l)]
    }
}

impl NodeId {
    /// Post-increment (e.g. `self++`).
    fn bump(&mut self) -> NodeId {
        let ret = *self;
        self.0 += 1;
        ret
    }
}

impl From<NodeId> for usize {
    fn from(NodeId(id): NodeId) -> usize {
        id as usize
    }
}