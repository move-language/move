//! A `Cursor` wraps an in-memory buffer and provides it with a stripped
//! down version of the `std::io::Cursor`.

use core::cmp;

use anyhow::{Error, Result};

/// A `Cursor` wraps an in-memory buffer and provides it with a stripped
/// down version of the `std::io::Cursor`.
#[derive(Clone, Debug, Default, Eq, PartialEq)]
pub struct Cursor<T>
where
    T: AsRef<[u8]>,
{
    inner: T,
    pos: u64,
}

impl<T> Cursor<T>
where
    T: AsRef<[u8]>,
{
    /// Creates a new cursor wrapping the provided underlying in-memory buffer.
    ///
    /// Cursor initial position is `0` even if underlying buffer (e.g., `Vec`)
    /// is not empty. So writing to cursor starts with overwriting `Vec`
    /// content, not with appending to it.
    pub fn new(inner: T) -> Cursor<T> {
        Cursor { pos: 0, inner }
    }

    /// Gets a reference to the underlying value in this cursor.
    pub fn get_ref(&self) -> &T {
        &self.inner
    }

    /// Returns the current position of this cursor.
    pub fn position(&self) -> u64 {
        self.pos
    }

    /// Pull some bytes from this source into the specified buffer, returning
    /// how many bytes were read.
    pub fn read(&mut self, buf: &mut [u8]) -> Result<usize> {
        let source = self.fill_buf();
        let amt = cmp::min(buf.len(), source.len());

        if amt == 1 {
            buf[0] = source[0];
        } else {
            buf[..amt].copy_from_slice(&source[..amt]);
        }

        self.pos += amt as u64;
        Ok(amt)
    }

    /// Read the exact number of bytes required to fill `buf`.
    ///
    /// This function reads as many bytes as necessary to completely fill the
    /// specified buffer `buf`.
    pub fn read_exact(&mut self, buf: &mut [u8]) -> Result<()> {
        let n = buf.len();
        let source = self.fill_buf();

        if buf.len() > source.len() {
            return Err(Error::msg("Unexpected Eof failed to fill whole buffer"));
        }

        if buf.len() == 1 {
            buf[0] = source[0];
        } else {
            buf.copy_from_slice(&source[..n]);
        }

        self.pos += n as u64;
        Ok(())
    }

    fn fill_buf(&mut self) -> &[u8] {
        let amt = cmp::min(self.pos, self.inner.as_ref().len() as u64);
        &self.inner.as_ref()[(amt as usize)..]
    }
}

#[cfg(test)]
mod tests {
    use super::Cursor;

    #[test]
    fn test_mem_reader() {
        let mut reader = Cursor::new(vec![0, 1, 2, 3, 4, 5, 6, 7]);
        let mut buf = [];
        assert_eq!(reader.read(&mut buf).unwrap(), 0);
        assert_eq!(reader.position(), 0);
        let mut buf = [0];
        assert_eq!(reader.read(&mut buf).unwrap(), 1);
        assert_eq!(reader.position(), 1);
        let b: &[_] = &[0];
        assert_eq!(buf, b);
        let mut buf = [0; 4];
        assert_eq!(reader.read(&mut buf).unwrap(), 4);
        assert_eq!(reader.position(), 5);
        let b: &[_] = &[1, 2, 3, 4];
        assert_eq!(buf, b);
        assert_eq!(reader.read(&mut buf).unwrap(), 3);
        let b: &[_] = &[5, 6, 7];
        assert_eq!(&buf[..3], b);
        assert_eq!(reader.read(&mut buf).unwrap(), 0);
    }

    #[test]
    fn test_boxed_slice_reader() {
        let mut reader = Cursor::new(vec![0, 1, 2, 3, 4, 5, 6, 7].into_boxed_slice());
        let mut buf = [];
        assert_eq!(reader.read(&mut buf).unwrap(), 0);
        assert_eq!(reader.position(), 0);
        let mut buf = [0];
        assert_eq!(reader.read(&mut buf).unwrap(), 1);
        assert_eq!(reader.position(), 1);
        let b: &[_] = &[0];
        assert_eq!(buf, b);
        let mut buf = [0; 4];
        assert_eq!(reader.read(&mut buf).unwrap(), 4);
        assert_eq!(reader.position(), 5);
        let b: &[_] = &[1, 2, 3, 4];
        assert_eq!(buf, b);
        assert_eq!(reader.read(&mut buf).unwrap(), 3);
        let b: &[_] = &[5, 6, 7];
        assert_eq!(&buf[..3], b);
        assert_eq!(reader.read(&mut buf).unwrap(), 0);
    }

    #[test]
    fn test_buf_reader() {
        let in_buf = vec![0, 1, 2, 3, 4, 5, 6, 7];
        let mut reader = Cursor::new(&in_buf[..]);
        let mut buf = [];
        assert_eq!(reader.read(&mut buf).unwrap(), 0);
        assert_eq!(reader.position(), 0);
        let mut buf = [0];
        assert_eq!(reader.read(&mut buf).unwrap(), 1);
        assert_eq!(reader.position(), 1);
        let b: &[_] = &[0];
        assert_eq!(buf, b);
        let mut buf = [0; 4];
        assert_eq!(reader.read(&mut buf).unwrap(), 4);
        assert_eq!(reader.position(), 5);
        let b: &[_] = &[1, 2, 3, 4];
        assert_eq!(buf, b);
        assert_eq!(reader.read(&mut buf).unwrap(), 3);
        let b: &[_] = &[5, 6, 7];
        assert_eq!(&buf[..3], b);
        assert_eq!(reader.read(&mut buf).unwrap(), 0);
    }
}
