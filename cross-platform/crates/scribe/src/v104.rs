mod records {
    use binrw::{BinRead, BinWrite};
    use bitflags::bitflags;

    use crate::bitflags_brw;

    #[derive(Debug, BinRead, BinWrite, Clone)]
    #[brw(magic = b"BSA\0")]
    pub struct HeaderRecord {
        #[brw(assert(version.eq(&104)))]
        pub version: u32,
        pub folder_offset: u32,
        pub archive_flags: ArchiveFlags,
        pub folder_count: u32,
        pub file_count: u32,
        pub total_folder_name_length: u32,
        pub total_file_name_length: u32,
        pub content_flags: ContentFlags,
        pub padding: u16,
    }

    #[derive(Debug, BinRead, BinWrite, Clone)]
    pub struct FileRecord {
        pub hash: u64,
        pub size: u32,
        pub offset: u32,
    }

    #[derive(Debug, BinRead, BinWrite, Clone)]
    pub struct FolderRecord {
        pub hash: u64,
        pub file_count: u32,
        pub offset: u32,
    }

    impl HeaderRecord {
        pub fn get_offset_to_file_names(&self) -> u32 {
            let extra_len = if self.archive_flags.contains(ArchiveFlags::DIR_NAMES) {
                self.total_folder_name_length + self.folder_count
            } else {
                0
            };
            self.folder_offset + self.folder_count * 16 + extra_len + self.file_count * 16
        }
    }

    bitflags! {
        #[derive(Debug, Clone, Copy)]
        pub struct ArchiveFlags: u32 {
            const DIR_NAMES = 0x1;
            const FILE_NAMES = 0x2;
            const INVERT_COMPRESSION = 0x4;
            const RETAIN_DIR_NAMES = 0x8;
            const RETAIN_FILE_NAMES = 0x10;
            const RETAIN_FILE_NAME_OFFSETS = 0x20;
            const BIG_ENDIAN = 0x40;
            const RETAIN_STRINGS = 0x80;
            const EMBED_FILE_NAMES = 0x100;
            const XMEM_CODEC = 0x200; // unsupported!
        }
    }
    bitflags_brw!(ArchiveFlags: u32);

    impl ArchiveFlags {
        pub const UNSUPPORTED_UNION: Self = ArchiveFlags::XMEM_CODEC;
    }

    bitflags! {
        #[derive(Debug, Clone, Copy)]
        pub struct ContentFlags: u16 {
            const MESHES = 0x1;
            const TEXTURES = 0x2;
            const MENUS = 0x4;
            const SOUNDS = 0x8;
            const VOICES = 0x10;
            const SHADERS = 0x20;
            const TREES = 0x40;
            const FONTS = 0x80;
            const MISC = 0x100;
        }
    }
    bitflags_brw!(ContentFlags: u16);
}

mod read {
    use core::slice;
    use std::io::{
        prelude::{Read, Seek},
        SeekFrom::{Current, Start},
    };

    use binrw::{BinRead, BinResult, Endian, NullString};
    use flate2::read::ZlibDecoder;

    use crate::{
        common::polonius_hack::{polo_ref, Polo},
        Archive, Entry,
    };

    use super::records::*;

    pub struct Rcx<R> {
        read: R,
        header: HeaderRecord,
        folder_paths: Vec<String>,
        file_names: Vec<String>,
        last_file_name_offset: u64,
    }

    #[derive(Debug, Clone)]
    pub struct RootPtr {
        cumulative_files_before: usize,
        base_offset: u32,
        folder_index: usize,
    }

    #[derive(Debug, Clone)]
    pub struct InFolderPtr {
        cumulative_files_before: usize,
        file_count: u32,
        base_offset: u64,
        name_len: u8,
        folder_index: usize,
        file_index: usize,
    }

    #[derive(Debug, Clone)]
    pub enum FolderPtr {
        Root(RootPtr),
        Folder(InFolderPtr),
    }

    #[derive(Debug, Clone)]
    pub struct FilePtr {
        cumulative_files_before: usize,
        size: u32,
        offset_of_file_data: u32,
        compressed: bool,
    }

    #[derive(Debug)]
    pub struct Meta {
        pub folder_count: usize,
        pub file_count: usize,
        pub header: super::Header,
    }

    impl<R: Read + Seek> Rcx<R> {
        fn endian(&self) -> Endian {
            if self.header.archive_flags.contains(ArchiveFlags::BIG_ENDIAN) {
                Endian::Big
            } else {
                Endian::Little
            }
        }

        fn has_file_names(&self) -> bool {
            self.header.archive_flags.contains(ArchiveFlags::FILE_NAMES)
        }

        fn has_folder_names(&self) -> bool {
            self.header.archive_flags.contains(ArchiveFlags::DIR_NAMES)
        }

        fn read<T: for<'a> BinRead<Args<'a> = ()>>(&mut self) -> BinResult<T> {
            let ed = self.endian();
            T::read_options(&mut self.read, ed, ())
        }

        fn goto(&mut self, pos: u64) -> BinResult<u64> {
            self.read.seek(Start(pos)).map_err(From::from)
        }

        fn fill_file_names(&mut self, names_needed: usize) -> BinResult<Option<&str>> {
            self.file_names.reserve(names_needed);

            let start_idx =
                self.header.get_offset_to_file_names() as u64 + self.last_file_name_offset;

            self.read.seek(Start(start_idx))?;

            for _ in 0..names_needed {
                let name: NullString = self.read()?;

                let name_len = name.len() as u64 + 1;
                self.last_file_name_offset += name_len;
                let name = name.try_into().or_else(|err| {
                    Err(binrw::Error::Custom {
                        pos: self.read.stream_position()? - name_len,
                        err: Box::new(err),
                    })
                })?;

                self.file_names.push(name);
            }

            Ok(self.file_names.last().map(|s| s.as_str()))
        }

        fn next_folder(&mut self, ptr: &mut RootPtr) -> BinResult<Option<FolderPtr>> {
            if ptr.folder_index >= self.header.folder_count as usize {
                return Ok(None);
            }

            self.goto(ptr.base_offset as u64 + (ptr.folder_index * 16) as u64)?;
            let folder: FolderRecord = self.read()?;

            let mut offset = (folder.offset - self.header.total_file_name_length) as u64;

            let name_len = if self.has_folder_names() {
                self.read.seek(Start(offset))?;
                let len: u8 = self.read()?;
                offset += len as u64 + 1;

                len + 1
            } else {
                0
            };

            let folder = InFolderPtr {
                file_count: folder.file_count,
                base_offset: offset,
                file_index: 0,
                name_len,
                cumulative_files_before: ptr.cumulative_files_before,
                folder_index: ptr.folder_index,
            };

            ptr.folder_index += 1;
            ptr.cumulative_files_before += folder.file_count as usize;

            Ok(Some(FolderPtr::Folder(folder)))
        }

        fn next_file(&mut self, ptr: &mut FolderPtr) -> BinResult<Option<FilePtr>> {
            let FolderPtr::Folder(ptr) = ptr else {
                return Ok(None);
            };

            if ptr.file_index >= ptr.file_count as usize {
                return Ok(None);
            }

            self.goto(ptr.base_offset as u64 + (ptr.file_index * 16) as u64)?;

            let FileRecord { size, offset, .. } = self.read()?;
            let flags = size >> 30;
            let size = size & (1 << 30) - 1;

            let file = FilePtr {
                size,
                offset_of_file_data: offset,
                compressed: (flags & 1) != 0,
                cumulative_files_before: ptr.cumulative_files_before,
            };

            ptr.file_index += 1;
            ptr.cumulative_files_before += 1;

            Ok(Some(file))
        }

        fn root(&mut self) -> BinResult<RootPtr> {
            let ptr = RootPtr {
                cumulative_files_before: 0,
                base_offset: self.header.folder_offset,
                folder_index: 0,
            };
            Ok(ptr)
        }
    }

    pub fn read<R: Read + Seek>(mut read: R) -> BinResult<(Rcx<R>, Meta)> {
        read.rewind()?;
        let header = HeaderRecord::read_le(&mut read)?;

        let intersection = header
            .archive_flags
            .intersection(ArchiveFlags::UNSUPPORTED_UNION);

        if !intersection.is_empty() {
            return Err(binrw::Error::AssertFail {
                pos: 8,
                message: format!("archive flags {intersection:?} are not supported",),
            });
        }

        let meta = Meta {
            file_count: header.file_count as usize,
            folder_count: header.folder_count as usize,
            header: header.clone(),
        };

        let cx = Rcx {
            read,
            header,
            folder_paths: Vec::new(),
            file_names: Vec::new(),
            last_file_name_offset: 0,
        };

        Ok((cx, meta))
    }

    impl<R: Read + Seek> Archive for Rcx<R> {
        type FolderPtr = FolderPtr;
        type FilePtr = FilePtr;

        type Err = binrw::Error;

        fn root_folder(&mut self) -> BinResult<FolderPtr> {
            self.root().map(FolderPtr::Root)
        }

        fn next_entry(&mut self, ptr: &mut FolderPtr) -> BinResult<Option<Entry<Self>>> {
            match ptr {
                FolderPtr::Root(root) => self.next_folder(root).map(|slot| slot.map(Entry::Folder)),
                FolderPtr::Folder(_) => self.next_file(ptr).map(|slot| slot.map(Entry::File)),
            }
        }

        fn read_file_contents<'a>(&'a mut self, ptr: &Self::FilePtr) -> BinResult<impl Read + 'a> {
            self.read.seek(Start(ptr.offset_of_file_data as u64))?;

            if self
                .header
                .archive_flags
                .contains(ArchiveFlags::EMBED_FILE_NAMES)
            {
                let mut name_len = 0;
                self.read.read_exact(slice::from_mut(&mut name_len))?;
                self.read.seek(Current(name_len as i64))?;
            }

            /// People read it; contents change on a dime.
            enum Tabloid<A, B> {
                A(A),
                B(B),
            }

            impl<A: Read, B: Read> Read for Tabloid<A, B> {
                #[inline]
                fn read(&mut self, buf: &mut [u8]) -> std::io::Result<usize> {
                    match self {
                        Tabloid::A(a) => a.read(buf),
                        Tabloid::B(b) => b.read(buf),
                    }
                }
            }

            let is_compressed = self
                .header
                .archive_flags
                .contains(ArchiveFlags::INVERT_COMPRESSION)
                ^ ptr.compressed;

            let tabloid = if !is_compressed {
                let base = self.read.by_ref().take(ptr.size as u64);
                Tabloid::A(base)
            } else {
                let _original_size: u32 = self.read()?;
                let base = self.read.by_ref().take(ptr.size as u64 - 4);
                Tabloid::B(ZlibDecoder::new(base))
            };

            Ok(tabloid)
        }

        fn folder_path<'a>(&'a mut self, ptr: &FolderPtr) -> BinResult<Option<&'a str>> {
            let FolderPtr::Folder(ptr) = ptr else {
                return Ok(Some(""));
            };

            if self.has_folder_names() {
                let offset = ptr.base_offset - ptr.name_len as u64 + 1;
                self.read.seek(Start(offset))?;

                let name: NullString = self.read()?;

                let name_len = name.len() as u64 + 1;
                let name = name.try_into().or_else(|err| {
                    Err(binrw::Error::Custom {
                        pos: self.read.stream_position()? - name_len,
                        err: Box::new(err),
                    })
                })?;

                self.folder_paths.insert(ptr.folder_index, name);
            };

            let s = self.folder_paths.get(ptr.folder_index).map(String::as_str);
            Ok(s)
        }

        fn file_name<'a>(&'a mut self, file: &FilePtr) -> BinResult<Option<&'a str>> {
            let polo = polo_ref(self, move |cx| {
                cx.file_names
                    .get(file.cumulative_files_before)
                    .map(|s| s.as_str())
            });

            let cx = match polo {
                Polo::Borrower(r) => return Ok(Some(r)),
                Polo::Lender(this) => this,
            };

            let names_needed = file.cumulative_files_before - cx.file_names.len() + 1;
            cx.fill_file_names(names_needed)
        }

        fn should_compress(&self, file: &Self::FilePtr) -> Result<Option<bool>, Self::Err> {
            let flag = self
                .header
                .archive_flags
                .contains(ArchiveFlags::INVERT_COMPRESSION)
                ^ file.compressed;
            Ok(Some(flag))
        }
    }
}

mod write {
    use std::{
        io::{
            self,
            prelude::Seek,
            SeekFrom::{Current, Start},
            Write,
        },
        mem,
    };

    use core::fmt::Debug;

    use binrw::{BinResult, BinWrite, Endian};
    use flate2::{write::ZlibEncoder, Compression};

    use crate::{Archive, Entry};

    use super::{hash::hash, records::*};

    pub struct Wcx<W> {
        write: W,
        cfg: Wcfg,
    }

    impl<W: Write + Seek> Wcx<W> {
        fn write<T: for<'a> BinWrite<Args<'a> = ()>>(&mut self, v: T) -> BinResult<()> {
            let endian = if self.cfg.big_endian {
                Endian::Big
            } else {
                Endian::Little
            };

            v.write_options(&mut self.write, endian, ())
        }
    }

    pub struct Wcfg {
        pub big_endian: bool,
        pub flags: ArchiveFlags,
    }

    impl Default for Wcfg {
        fn default() -> Wcfg {
            Wcfg {
                big_endian: false,
                flags: ArchiveFlags::DIR_NAMES | ArchiveFlags::FILE_NAMES,
            }
        }
    }
    pub fn write<A: Archive, W: Write + Seek>(archive: &mut A, write: W, cfg: Wcfg) -> BinResult<()>
    where
        // fixme: change api to avoid this ridiculousness.
        binrw::Error: From<A::Err>,
        A::FolderPtr: Debug,
        A::FilePtr: Debug,
    {
        let mut cx = Wcx { write, cfg };

        #[derive(Debug)]
        struct FolderThings {
            info: FolderRecord,
            total_path: String,
        }

        impl Default for FolderThings {
            fn default() -> Self {
                FolderThings {
                    info: FolderRecord {
                        hash: 0,
                        file_count: 0,
                        offset: 36,
                    },
                    total_path: String::new(),
                }
            }
        }

        // we don't have enough information to write out the header yet,
        // so we write zeros and come back later.
        cx.write.write_all(&[0; 36])?;

        let mut folders = Vec::new();
        let mut files = Vec::new();

        {
            let mut this_ptr = archive.root_folder()?;
            let mut things = FolderThings::default();
            let mut folder_stack = Vec::new();
            let mut file_buf = Vec::new();

            loop {
                while let Some(entry) = archive.next_entry(&mut this_ptr)? {
                    match entry {
                        Entry::Folder(dir) => {
                            let mut total_path = things.total_path.clone();
                            folder_stack.push((
                                this_ptr,
                                mem::take(&mut things),
                                mem::take(&mut file_buf),
                            ));

                            let path = archive.folder_path(&dir)?.unwrap();
                            things.info.hash = hash(path);
                            if cx.cfg.flags.contains(ArchiveFlags::DIR_NAMES) {
                                if !total_path.is_empty() {
                                    total_path.reserve(path.len() + 1);
                                    total_path.push('\\');
                                }
                                total_path.push_str(path);
                            }

                            things.total_path = total_path;
                            this_ptr = dir;
                        }
                        Entry::File(file) => {
                            file_buf.push((
                                file,
                                FileRecord {
                                    hash: 0,
                                    size: 0,
                                    offset: 36,
                                },
                            ));
                            things.info.file_count += 1;
                        }
                    }
                }

                // like above, we don't bother with offsets/sizes/hash yet.
                cx.write.write_all(&[0; 16])?;

                folders.push((this_ptr, things));
                files.extend(file_buf);

                let Some((old_ptr, old_things, old_files)) = folder_stack.pop() else {
                    break;
                };

                file_buf = old_files;
                this_ptr = old_ptr;
                things = old_things;
            }
        }

        let mut total_file_records_offset = 16 * folders.len();
        let mut total_folder_name_length = 0;

        for (_, things) in &mut folders {
            things.info.offset += total_file_records_offset as u32;

            let str_len: u8 =
                things
                    .total_path
                    .len()
                    .try_into()
                    .map_err(|_| binrw::Error::AssertFail {
                        pos: 0,
                        message: format!(
                            "folder path {path} must be less than 255 bytes in length.",
                            path = things.total_path,
                        ),
                    })?;
            total_file_records_offset += str_len as usize + 2;
            total_folder_name_length += str_len as usize + 1;

            cx.write.write_all(&[str_len + 1])?;
            cx.write.write_all(things.total_path.as_bytes())?;
            cx.write.write_all(&[0])?;

            for _ in 0..things.info.file_count {
                cx.write.write_all(&[0; 16])?;
                total_file_records_offset += 16;
            }
        }
        let mut total_file_name_length = 0;

        // third pass --- file names:
        if cx.cfg.flags.contains(ArchiveFlags::FILE_NAMES) {
            for (file, info) in files.iter_mut().map(|(f, i)| (&*f, i)) {
                // fixme!
                let name = archive.file_name(file)?.unwrap();
                cx.write.write_all(name.as_bytes())?;
                cx.write.write_all(&[0])?;

                info.hash = hash(name);

                total_file_name_length += name.len() + 1;
            }
        }

        // fourth pass --- file contents:
        let mut total_file_data_size: usize = 0;

        let embed_file_names = cx.cfg.flags.contains(ArchiveFlags::EMBED_FILE_NAMES);
        eprintln!("should embed: {embed_file_names}");

        let mut file_iter = files.iter_mut().map(|(f, i)| (&*f, i));
        for (_, things) in &folders {
            let folder_path = if embed_file_names {
                // FIXME: needs to nest with parent folders!!
                things.total_path.as_str()
            } else {
                ""
            };
            for (file, info) in file_iter.by_ref().take(things.info.file_count as usize) {
                info.offset += (total_file_records_offset
                    + total_file_name_length
                    + total_file_data_size) as u32;

                if embed_file_names {
                    let file_name = archive.file_name(file)?.unwrap();
                    let byte_len = (folder_path.len() + 1 + file_name.len()) as u8;

                    cx.write.write_all(&[byte_len])?;
                    cx.write.write_all(folder_path.as_bytes())?;
                    cx.write.write_all(b"\\")?;
                    cx.write.write_all(file_name.as_bytes())?;
                }

                let invert_compression = cx.cfg.flags.contains(ArchiveFlags::INVERT_COMPRESSION);
                let compress_data = match (archive.should_compress(file)?, invert_compression) {
                    (None, compress) => compress,
                    (Some(compress), _) => compress,
                };

                let compression_bit = {
                    let the_bit = (compress_data ^ invert_compression) as u32;
                    the_bit << 29
                };

                let mut data = archive.read_file_contents(file)?;

                let size = if compress_data {
                    eprintln!("ohn o! compressing");
                    // we allocate for the "original size" of the data.
                    cx.write.write_all(&[0; 4])?;

                    let start_pos = cx.write.stream_position()?;

                    let mut encoder = ZlibEncoder::new(cx.write, Compression::new(4));
                    let original_size = io::copy(&mut data, &mut encoder)?;
                    cx.write = encoder.finish()?;

                    let end_pos = cx.write.stream_position()?;

                    cx.write.seek(Start(start_pos - 4))?;
                    cx.write(original_size)?;

                    end_pos - start_pos
                } else {
                    io::copy(&mut data, &mut cx.write)?
                };

                total_file_data_size += size as usize;

                info.size = size as u32 | compression_bit;
            }
        }

        // phase two --- we go back over
        cx.write.seek(Start(0))?;

        let header = HeaderRecord {
            version: 104,
            folder_offset: 36,
            archive_flags: cx.cfg.flags,
            folder_count: folders.len() as u32,
            file_count: files.len() as u32,
            total_folder_name_length: total_folder_name_length as u32,
            total_file_name_length: total_file_name_length as u32,
            content_flags: ContentFlags::empty(),
            padding: 0,
        };
        header.write_le(&mut cx.write)?;

        for (_, things) in &mut folders {
            // you poor, poor thing.
            things.info.offset += total_file_name_length as u32;
            cx.write(&things.info)?;
        }

        let mut file_iter = files.iter_mut().map(|(f, i)| (&*f, i));
        for (_, things) in &folders {
            cx.write.seek(Current(things.total_path.len() as i64 + 2))?;

            for (ptr, info) in file_iter.by_ref().take(things.info.file_count as usize) {
                let name = archive.file_name(ptr)?;
                eprintln!("{name:?} -> {info:?}");
                cx.write(&*info)?;
            }
        }

        Ok(())
    }

    // pub fn write<A: Archive, W: Write + Seek>(archive: &mut A, write: W, cfg: Wcfg) -> BinResult<()>
    // where
    //     // fixme: change api to avoid this ridiculousness.
    //     binrw::Error: From<A::Err>,
    // {
    //     let mut cx = Wcx { write, cfg };

    //     // we don't have enough information to write out the header yet,
    //     // so we write zeros and come back later.
    //     cx.write.write_all(&[0; 36])?;

    //     // first pass --- we collect the folders since we need their count:
    //     let mut root = archive.root()?;
    //     let mut folders = Vec::new();
    //     let mut files = Vec::new();
    //     while let Some(folder) = archive.next_folder(&mut root)? {
    //         folders.push((
    //             folder,
    //             FolderRecord {
    //                 hash: 0,
    //                 file_count: 0,
    //                 offset: 36,
    //             },
    //             0,
    //         ));

    //         // like above, we don't bother with offsets/sizes/hash yet.
    //         cx.write.write_all(&[0; 16])?;
    //     }

    //     // second pass --- we write folder names & allocate file space (file record blocks):
    //     #[derive(Clone, Copy)]
    //     enum PathPresence {
    //         Unknown,
    //         AllPresent,
    //         AllAbsent,
    //     }

    //     let mut total_file_records_offset = 16 * folders.len();
    //     let mut total_folder_name_length = 0;

    //     let mut folder_paths = PathPresence::Unknown;
    //     for (mut folder, info, name_len) in folders.iter_mut().map(|(f, i, n)| (f.clone(), i, n)) {
    //         info.offset += total_file_records_offset as u32;

    //         match (archive.folder_path(&folder)?, folder_paths) {
    //             (None, PathPresence::AllPresent) | (Some(_), PathPresence::AllAbsent) => {
    //                 return Err(binrw::Error::AssertFail {
    //                     pos: 0,
    //                     message: "either exactly all or exactly no folders should have paths"
    //                         .to_owned(),
    //                 })
    //             }
    //             (None, PathPresence::AllAbsent) => (),
    //             (None, PathPresence::Unknown) => folder_paths = PathPresence::AllAbsent,
    //             (Some(path), PathPresence::AllPresent | PathPresence::Unknown) => {
    //                 let str_len: u8 =
    //                     path.len()
    //                         .try_into()
    //                         .map_err(|_| binrw::Error::AssertFail {
    //                             pos: 0,
    //                             message: format!(
    //                                 "folder path {path} must be less than 255 bytes in length."
    //                             ),
    //                         })?;
    //                 folder_paths = PathPresence::AllPresent;

    //                 *name_len = str_len as usize + 2;
    //                 total_file_records_offset += *name_len;
    //                 total_folder_name_length += str_len as usize + 1;
    //                 info.hash = hash(&path);

    //                 cx.write.write_all(&[str_len + 1])?;
    //                 cx.write.write_all(path.as_bytes())?;
    //                 cx.write.write_all(&[0])?;
    //             }
    //         }

    //         while let Some(file) = archive.next_file(&mut folder)? {
    //             cx.write.write_all(&[0; 16])?;
    //             files.push((
    //                 file,
    //                 FileRecord {
    //                     hash: 0,
    //                     size: 0,
    //                     offset: 36,
    //                 },
    //             ));

    //             info.file_count += 1;
    //             total_file_records_offset += 16;
    //         }
    //     }

    //     let mut total_file_name_length = 0;

    //     // third pass --- file names:
    //     if cx.cfg.flags.contains(ArchiveFlags::FILE_NAMES) {
    //         for (file, info) in files.iter_mut().map(|(f, i)| (&*f, i)) {
    //             // fixme!
    //             let name = archive.file_name(file)?.unwrap();
    //             cx.write.write_all(name.as_bytes())?;
    //             cx.write.write_all(&[0])?;

    //             info.hash = hash(name);

    //             total_file_name_length += name.len() + 1;
    //         }
    //     }

    //     // fourth pass --- file contents:
    //     let mut total_file_data_size: usize = 0;

    //     let mut file_iter = files.iter_mut().map(|(f, i)| (&*f, i));

    //     let embed_file_names = cx.cfg.flags.contains(ArchiveFlags::EMBED_FILE_NAMES);

    //     for (folder, dir_info, _) in &folders {
    //         let folder_path = if embed_file_names {
    //             archive.folder_path(folder)?.unwrap().to_owned()
    //         } else {
    //             String::new()
    //         };
    //         for _ in 0..dir_info.file_count {
    //             let Some((file, info)) = file_iter.next() else {
    //                 break;
    //             };

    //             info.offset += (total_file_records_offset + total_file_data_size) as u32;

    //             if embed_file_names {
    //                 let file_name = archive.file_name(file)?.unwrap();
    //                 let byte_len = (folder_path.len() + 1 + file_name.len()) as u8;

    //                 cx.write.write_all(&[byte_len])?;
    //                 cx.write.write_all(folder_path.as_bytes())?;
    //                 cx.write.write_all(b"\\")?;
    //                 cx.write.write_all(file_name.as_bytes())?;
    //             }

    //             let invert_compression = cx.cfg.flags.contains(ArchiveFlags::INVERT_COMPRESSION);
    //             let compress_data = match (archive.should_compress(file)?, invert_compression) {
    //                 (None, compress) => compress,
    //                 (Some(compress), _) => compress,
    //             };

    //             let compression_bit = {
    //                 let the_bit = (compress_data ^ invert_compression) as u32;
    //                 the_bit << 29
    //             };

    //             let mut data = archive.read_file_contents(file)?;

    //             let size = if compress_data {
    //                 // we allocate for the "original size" of the data.
    //                 cx.write.write_all(&[0; 4])?;

    //                 let start_pos = cx.write.stream_position()?;

    //                 let mut encoder = ZlibEncoder::new(cx.write, Compression::new(4));
    //                 let original_size = io::copy(&mut data, &mut encoder)?;
    //                 cx.write = encoder.finish()?;

    //                 let end_pos = cx.write.stream_position()?;

    //                 cx.write.seek(Start(start_pos - 4))?;
    //                 cx.write(original_size)?;

    //                 end_pos - start_pos
    //             } else {
    //                 io::copy(&mut data, &mut cx.write)?
    //             };

    //             total_file_data_size += size as usize;

    //             info.size = size as u32 | compression_bit;
    //         }
    //     }

    //     // phase two --- we go back over
    //     cx.write.seek(Start(0))?;

    //     let header = HeaderRecord {
    //         version: 104,
    //         folder_offset: 36,
    //         archive_flags: cx.cfg.flags,
    //         folder_count: folders.len() as u32,
    //         file_count: files.len() as u32,
    //         total_folder_name_length: total_folder_name_length as u32,
    //         total_file_name_length: total_file_name_length as u32,
    //         content_flags: ContentFlags::empty(),
    //         padding: 0,
    //     };
    //     header.write_le(&mut cx.write)?;

    //     for mut info in folders.iter().map(|(_, i, _)| i.clone()) {
    //         // you poor, poor thing.
    //         info.offset += total_file_name_length as u32;
    //         cx.write(info)?;
    //     }

    //     let mut file_idx = 0;
    //     for (_, info, &name_len) in &folders {
    //         cx.write.seek(Current(name_len as i64))?;

    //         let slice = &files[file_idx..][0..info.file_count as usize];
    //         for (_, info) in slice {
    //             cx.write(info)?;
    //         }

    //         file_idx += info.file_count as usize;
    //     }

    //     Ok(())
    // }
}

mod hash {
    pub fn hash(name: &str) -> u64 {
        let (stem, ext) = {
            let idx = name.find('.').unwrap_or(name.len());
            name.split_at(idx)
        };

        let mut b = u32::from_le_bytes([
            stem.as_bytes()
                .last()
                .map(u8::to_ascii_lowercase)
                .unwrap_or(0),
            stem.len()
                .checked_sub(2)
                .and_then(|idx| stem.as_bytes().get(idx))
                .map(u8::to_ascii_lowercase)
                .unwrap_or(0),
            stem.len() as u8,
            stem.as_bytes()
                .first()
                .map(u8::to_ascii_lowercase)
                .unwrap_or(0),
        ]);

        match ext {
            // this is actually vulgar:
            s if unicase::eq_ascii(s, ".kf") => b |= 0x80,
            s if unicase::eq_ascii(s, ".nif") => b |= 0x8000,
            s if unicase::eq_ascii(s, ".dds") => b |= 0x8080,
            s if unicase::eq_ascii(s, ".wav") => b |= 0x80000000,
            _ => (),
        }

        let mut stem_hash = 0u32;

        if stem.len() > 1 {
            for &byte in
                &stem.as_bytes()[1..stem.len().checked_sub(2).unwrap_or_else(|| stem.len() - 1)]
            {
                stem_hash = stem_hash
                    .wrapping_mul(0x1003f)
                    .wrapping_add(byte.to_ascii_lowercase() as u32);
            }
        }

        let mut ext_hash = 0u32;
        for &byte in ext.as_bytes() {
            ext_hash = ext_hash
                .wrapping_mul(0x1003f)
                .wrapping_add(byte.to_ascii_lowercase() as u32);
        }

        let a = stem_hash.wrapping_add(ext_hash) as u64;
        (a << 32) + b as u64
    }
}

pub use read::{read, Meta};
pub use records::HeaderRecord as Header;
pub use write::{write, Wcfg};
