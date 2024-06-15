use std::{
    fs, io,
    path::{Path, PathBuf},
};

#[derive(argh::FromArgs)]
#[argh(description = "")]
struct Args {
    #[argh(subcommand)]
    command: Command,
}

#[derive(argh::FromArgs)]
#[argh(subcommand)]
enum Command {
    Compress(Compress),
    Decompress(Decompress),
}

#[derive(argh::FromArgs)]
#[argh(description = "")]
#[argh(subcommand, name = "compress")]
struct Compress {
    #[argh(positional)]
    #[argh(description = "input")]
    input: PathBuf,
    #[argh(option, short = 'o')]
    #[argh(description = "output")]
    output: PathBuf,
}

#[derive(argh::FromArgs)]
#[argh(description = "")]
#[argh(subcommand, name = "decompress")]
struct Decompress {
    #[argh(positional)]
    #[argh(description = "input")]
    input: PathBuf,
    #[argh(option, short = 'o')]
    #[argh(description = "output")]
    output: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let args: Args = argh::from_env();

    match args.command {
        Command::Compress(args) => compress(args),
        Command::Decompress(args) => decompress(args),
    }
}

use scribe::{v104, Archive, Entry};

fn compress(args: Compress) -> anyhow::Result<()> {
    enum DirEntry {
        Dir(DirInfo),
        File(PathBuf),
    }

    enum DirEntries {
        NotYet,
        Here(Vec<DirEntry>),
    }

    struct DirInfo {
        path: PathBuf,
        entries: DirEntries,
    }

    impl DirInfo {
        pub fn entries(&mut self) -> io::Result<&mut Vec<DirEntry>> {
            match &mut self.entries {
                slot @ DirEntries::NotYet => {
                    let mut entries = Vec::new();
                    for entry in fs::read_dir(&self.path)? {
                        let entry = entry?;
                        let is_dir = entry.file_type()?.is_dir();

                        let path = entry.path();
                        let entry = if is_dir {
                            DirEntry::Dir(DirInfo {
                                path,
                                entries: DirEntries::NotYet,
                            })
                        } else {
                            DirEntry::File(path)
                        };

                        entries.push(entry);
                    }

                    *slot = DirEntries::Here(entries);
                }
                DirEntries::Here(_) => (),
            }

            match &mut self.entries {
                DirEntries::NotYet => unreachable!(),
                DirEntries::Here(entries) => Ok(entries),
            }
        }

        pub fn get_relevant_dir<'a>(&'a mut self, ptr: &'a [usize]) -> io::Result<&'a mut DirInfo> {
            let mut dir = self;

            for seg in ptr {
                let entry = dir.entries()?.get_mut(*seg);
                match entry {
                    Some(DirEntry::Dir(d2)) => {
                        dir = d2;
                    }
                    Some(DirEntry::File(_)) => {
                        unreachable!("files cannot contain other dir entries!")
                    }
                    None => panic!("directory state has changed!"),
                }
            }

            Ok(dir)
        }
    }

    impl Archive for DirInfo {
        type FolderPtr = Vec<usize>;

        type FilePtr = PathBuf;

        type Err = io::Error;

        fn root_folder(&mut self) -> Result<Self::FolderPtr, Self::Err> {
            Ok(vec![0])
        }

        fn next_entry(
            &mut self,
            ptr: &mut Self::FolderPtr,
        ) -> Result<Option<Entry<Self>>, Self::Err> {
            let (entry_idx, path) = ptr.split_last_mut().unwrap();
            let dir = self.get_relevant_dir(path)?;

            let entry = dir.entries()?.get_mut(*entry_idx);
            *entry_idx += 1;
            match entry {
                Some(DirEntry::Dir(_)) => {
                    let mut next_ptr = ptr.clone();
                    *next_ptr.last_mut().unwrap() -= 1;
                    next_ptr.push(0);
                    Ok(Some(Entry::Folder(next_ptr)))
                }
                Some(DirEntry::File(p)) => Ok(Some(Entry::File(p.clone()))),
                None => Ok(None),
            }
        }

        fn read_file_contents<'a>(
            &'a mut self,
            ptr: &Self::FilePtr,
        ) -> Result<impl io::Read + 'a, Self::Err> {
            fs::File::open(ptr)
        }

        fn folder_path<'a>(
            &'a mut self,
            ptr: &'a Self::FolderPtr,
        ) -> Result<Option<&'a str>, Self::Err> {
            let (_, path) = ptr.split_last().unwrap();
            let dir = self.get_relevant_dir(path)?;

            Ok(dir.path.file_name().and_then(|str| str.to_str()))
        }

        fn file_name<'a>(
            &'a mut self,
            file: &'a Self::FilePtr,
        ) -> Result<Option<&'a str>, Self::Err> {
            Ok(file.file_name().and_then(|str| str.to_str()))
        }

        fn should_compress(&self, _: &Self::FilePtr) -> Result<Option<bool>, Self::Err> {
            Ok(None)
        }
    }

    let mut dir = DirInfo {
        path: args.input,
        entries: DirEntries::NotYet,
    };

    let outstream = fs::OpenOptions::new()
        .create_new(true)
        .write(true)
        .truncate(true)
        .open(&args.output)?;

    v104::write(&mut dir, outstream, v104::Wcfg::default())?;

    Ok(())
}

fn decompress(args: Decompress) -> anyhow::Result<()> {
    fn read_and_write<A: Archive>(
        parent: &Path,
        archive: &mut A,
        mut folder: A::FolderPtr,
    ) -> anyhow::Result<()>
    where
        anyhow::Error: From<A::Err>,
    {
        let path_ext = archive.folder_path(&folder)?.unwrap();
        let proper_path_ext = <PathBuf as path_slash::PathBufExt>::from_backslash(&path_ext);
        let path = parent.join(proper_path_ext);

        fs::create_dir_all(&path)?;

        while let Some(entry) = archive.next_entry(&mut folder)? {
            match entry {
                Entry::Folder(folder) => read_and_write(&path, archive, folder)?,
                Entry::File(file) => {
                    let file_name = archive.file_name(&file)?.unwrap();
                    let full_path = path.join(file_name);

                    let mut out = fs::OpenOptions::new()
                        .write(true)
                        .create(true)
                        .open(&full_path)?;

                    let mut contents = archive.read_file_contents(&file)?;

                    io::copy(&mut contents, &mut out)?;
                }
            }
        }

        Ok(())
    }

    let file = fs::File::open(&args.input)?;

    let (mut archive, ..) = v104::read(file)?;

    let root = archive.root_folder()?;
    read_and_write(&args.output, &mut archive, root)?;

    Ok(())
}
