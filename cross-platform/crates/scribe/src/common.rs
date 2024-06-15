use crate::{Archive, Entry};

#[macro_export]
macro_rules! bitflags_brw {
    ($ty:ty : $inner:ty) => {
        impl binrw::BinRead for $ty {
            type Args<'a> = ();

            fn read_options<R: std::io::Read + std::io::Seek>(
                reader: &mut R,
                endian: binrw::Endian,
                _: (),
            ) -> binrw::BinResult<Self> {
                <$inner as binrw::BinRead>::read_options(reader, endian, ())
                    .map(|x| x as _)
                    .map(Self::from_bits_truncate)
            }
        }

        impl binrw::BinWrite for $ty {
            type Args<'a> = ();

            fn write_options<W: std::io::Write + std::io::Seek>(
                &self,
                writer: &mut W,
                endian: binrw::Endian,
                _: (),
            ) -> binrw::BinResult<()> {
                <$inner as binrw::BinWrite>::write_options(
                    &core::convert::From::from(self.bits()),
                    writer,
                    endian,
                    (),
                )
            }
        }
    };
}

pub(crate) mod polonius_hack {
    pub enum Polo<'a, T: ?Sized, U> {
        Borrower(U),
        Lender(&'a mut T),
    }

    pub fn polo_ref<'a, T: ?Sized, U: ?Sized, F>(obj: &'a mut T, f: F) -> Polo<'a, T, &'a U>
    where
        F: for<'t> FnOnce(&'t mut T) -> Option<&'t U>,
    {
        let obj_ptr = obj as *mut _;
        let opt = f(obj);
        match opt {
            Some(u) => Polo::Borrower(u),
            None => {
                // SAFETY: no references to `obj` exist:
                // - no references are contained in the return value of `f`,
                // - and since we use HRTBs,
                // a shared reference cannot outlive the execution of `f`.
                let obj = unsafe { &mut *obj_ptr };
                Polo::Lender(obj)
            }
        }
    }

    pub fn polo_mut<'a, T: ?Sized, U: ?Sized, F>(obj: &'a mut T, f: F) -> Polo<'a, T, &'a mut U>
    where
        F: for<'t> FnOnce(&'t mut T) -> Option<&'t mut U>,
    {
        let obj_ptr = obj as *mut _;
        let opt = f(obj);
        match opt {
            Some(u) => Polo::Borrower(u),
            None => {
                // SAFETY: no references to `obj` exist:
                // - no references are contained in the return value of `f`,
                // - and since we use HRTBs,
                // a shared reference cannot outlive the execution of `f`.
                let obj = unsafe { &mut *obj_ptr };
                Polo::Lender(obj)
            }
        }
    }
}

#[doc(hidden)]
pub fn util_print_archive<A: Archive>(cx: &mut A) -> Result<(), A::Err> {
    fn walk<A: Archive>(cx: &mut A, mut ptr: A::FolderPtr, depth: usize) -> Result<(), A::Err> {
        while let Some(entry) = cx.next_entry(&mut ptr)? {
            match entry {
                Entry::Folder(folder) => {
                    let folder_name = cx.folder_path(&folder)?.unwrap();
                    print!("{}", "  ".repeat(depth));
                    println!(" >> {folder_name}");
                    walk(cx, folder, depth + 1)?;
                }
                Entry::File(file) => {
                    let file_name = cx.file_name(&file)?.unwrap();
                    print!("{}", "  ".repeat(depth));
                    println!(" ++ {file_name}");
                }
            }
        }

        Ok(())
    }
    let root = cx.root_folder()?;
    walk(cx, root, 0)
}
