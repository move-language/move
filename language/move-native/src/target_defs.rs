// Copyright (c) The Diem Core Contributors
// Copyright (c) The Move Contributors
// SPDX-License-Identifier: Apache-2.0

pub use impls::*;

#[cfg(not(feature = "solana"))]
mod impls {
    // Move addresses are 16 bytes by default, but can be made 20 or 32 at compile time.
    pub const ACCOUNT_ADDRESS_LENGTH: usize = 16;

    pub fn print_string(_s: &str) {
        todo!()
    }

    pub fn print_stack_trace() {
        todo!()
    }

    pub fn abort(_code: u64) -> ! {
        todo!()
    }
}

#[cfg(feature = "solana")]
mod impls {
    // Solana pubkeys are 32 bytes.
    // Move addresses are 16 bytes by default, but can be made 20 or 32 at compile time.
    pub const ACCOUNT_ADDRESS_LENGTH: usize = 32;

    pub fn print_string(s: &str) {
        unsafe {
            syscalls::sol_log_(s.as_ptr(), s.len() as u64);
        }
    }

    pub fn print_stack_trace() {
        todo!()
    }

    pub fn abort(code: u64) -> ! {
        unsafe {
            syscalls::sol_log_64_(
                code, code, code, code, code,
            );
            syscalls::abort()
        }
    }

    // NB: not using the "static-syscalls" sbf feature
    mod syscalls {
        extern "C" {
            pub fn abort() -> !;
            pub fn sol_log_(msg: *const u8, len: u64);
            pub fn sol_log_64_(_: u64, _: u64, _: u64, _: u64, _: u64);
        }
    }

    mod globals {
        use alloc::alloc::{GlobalAlloc, Layout};
        use alloc::format;
        use core::mem::size_of;
        use core::ptr::null_mut;

        const PANIC_ABORT_CODE: u64 = 101;

        #[panic_handler]
        fn panic(info: &core::panic::PanicInfo) -> ! {
            super::print_string(&format!("{}", info));
            super::abort(PANIC_ABORT_CODE);
        }

        #[global_allocator]
        static A: BumpAllocator = BumpAllocator {
            start: HEAP_START_ADDRESS as usize,
            len: HEAP_LENGTH,
        };

        pub struct BumpAllocator {
            pub start: usize,
            pub len: usize,
        }

        unsafe impl GlobalAlloc for BumpAllocator {
            #[inline]
            unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
                let pos_ptr = self.start as *mut usize;

                let mut pos = *pos_ptr;
                if pos == 0 {
                    // First time, set starting position
                    pos = self.start + self.len;
                }
                pos = pos.saturating_sub(layout.size());
                pos &= !(layout.align().wrapping_sub(1));
                if pos < self.start + size_of::<*mut u8>() {
                    return null_mut();
                }
                *pos_ptr = pos;
                pos as *mut u8
            }
            #[inline]
            unsafe fn dealloc(&self, _: *mut u8, _: Layout) {
                // I'm a bump allocator, I don't free
            }
        }
        pub const HEAP_START_ADDRESS: u64 = 0x300000000;
        pub const HEAP_LENGTH: usize = 32 * 1024;
    }
}
