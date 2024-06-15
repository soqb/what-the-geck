use std::io::Read;

use bitflags::bitflags;

#[cfg(feature = "v104")]
pub mod v104;

#[macro_use]
pub mod common;

pub struct NameHash {
    pub low: u32,
    pub high: u32,
}

bitflags! {
    #[derive(Debug, Clone, Copy)]
    struct ArchiveFlags: u64 {
        const DIR_NAMES = 0x1;
        const FILE_NAMES = 0x2;
        const COMPRESSED_BY_DEFAULT = 0x4;
        const BIG_ENDIAN = 0x40;
    }
}

#[derive(Debug, Clone)]
pub enum Entry<A: Archive + ?Sized> {
    Folder(A::FolderPtr),
    File(A::FilePtr),
}

pub trait Archive {
    type FolderPtr: Clone;
    type FilePtr: Clone;

    type Err;

    // fn root(&mut self) -> Result<Self::RootPtr, Self::Err>;
    fn root_folder(&mut self) -> Result<Self::FolderPtr, Self::Err>;

    fn next_entry(&mut self, ptr: &mut Self::FolderPtr) -> Result<Option<Entry<Self>>, Self::Err>;

    // fn next_folder(
    //     &mut self,
    //     ptr: &mut Self::RootPtr,
    // ) -> Result<Option<Self::FolderPtr>, Self::Err>;
    // fn next_file(&mut self, ptr: &mut Self::FolderPtr) -> Result<Option<Self::FilePtr>, Self::Err>;
    fn read_file_contents<'a>(
        &'a mut self,
        ptr: &Self::FilePtr,
    ) -> Result<impl Read + 'a, Self::Err>;

    fn folder_path<'a>(
        &'a mut self,
        folder: &'a Self::FolderPtr,
    ) -> Result<Option<&'a str>, Self::Err>;
    fn file_name<'a>(&'a mut self, file: &'a Self::FilePtr) -> Result<Option<&'a str>, Self::Err>;

    fn should_compress(&self, file: &Self::FilePtr) -> Result<Option<bool>, Self::Err>;
}

// pub trait BsaWrite {
//     type Cx<W: Write + Seek>;

//     type RootPtr: Clone;
//     type FolderPtr: Clone;
//     type FilePtr: Clone;
//     type FileWrite<'a, W: Write + 'a>: Write;

//     type Config;
//     type Err;

//     fn init_write_args<W: Write + Seek>(
//         write: W,
//         cfg: Self::Config,
//         header: Header,
//     ) -> Result<(Self::Cx<W>, Self::RootPtr), Self::Err>;
//     fn init_write<W: Write + Seek>(
//         write: W,
//         header: Header,
//     ) -> Result<(Self::Cx<W>, Self::RootPtr), Self::Err>
//     where
//         Self::Config: Default,
//     {
//         Self::init_write_args(write, Default::default(), header)
//     }

//     fn insert_folder<W: Write + Seek>(
//         cx: &mut Self::Cx<W>,
//         ptr: &mut Self::RootPtr,
//         path: &impl AsRef<str>,
//     ) -> Result<Self::FolderPtr, Self::Err>;

//     fn insert_file<W: Write + Seek>(
//         cx: &mut Self::Cx<W>,
//         ptr: &mut Self::FolderPtr,
//         filename: &impl AsRef<str>,
//     ) -> Result<Self::FilePtr, Self::Err>;

//     fn file_contents<'a, W: Write + Seek, R>(
//         cx: &'a mut Self::Cx<W>,
//         ptr: Self::FilePtr,
//         invert_compression: bool,
//         write: impl FnOnce(&mut Self::FileWrite<'a, W>) -> Result<R, Self::Err>,
//     ) -> Result<R, Self::Err>;

//     fn finish<W: Write + Seek>(cx: Self::Cx<W>) -> Result<W, Self::Err>;
// }

#[cfg(test)]
mod tests {
    use std::{fs, io};

    use binrw::BinResult;

    use crate::*;
    use crate::{common::util_print_archive, v104::Wcfg};

    // #[test]
    fn read_file_records() -> BinResult<()> {
        let path = "/home/seth/.local/share/Steam/steamapps/common/Fallout New Vegas/Data/MercenaryPack - Main.bsa";
        let stream = fs::File::open(path)?;

        let (mut arch, _meta) = v104::read(stream)?;

        util_print_archive(&mut arch)?;

        Ok(())
    }

    #[test]
    fn roundtrip() -> BinResult<()> {
        let bytes = {
            let path = "/home/seth/.local/share/Steam/steamapps/common/Fallout New Vegas/Data/LonesomeRoad - Main.bsa";
            let stream = fs::File::open(path)?;

            let (mut arch, _meta) = v104::read(stream)?;

            let mut cursor: io::Cursor<Vec<u8>> = Default::default();

            v104::write(&mut arch, &mut cursor, Wcfg::default())?;

            cursor.into_inner()
        };

        {
            let stream = io::Cursor::new(&bytes[..]);
            let (mut arch, _meta) = v104::read(stream)?;

            util_print_archive(&mut arch)?;
        }

        Ok(())
    }
}
