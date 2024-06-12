mod read_utils;

pub mod common;
pub mod format;
pub mod records;

#[cfg(test)]
mod tests {
    use std::{fs, io::Cursor};

    use binrw::BinRead;

    use crate::{
        common::Ident4,
        format::{MainFile, ParseOptions, PluginEntry, Record},
    };

    #[test]
    fn test_record_parsing() -> anyhow::Result<()> {
        // env_logger::Builder::from_default_env()
        //     .format_timestamp(None)
        //     .filter_level(log::LevelFilter::Trace)
        //     .target(env_logger::Target::Stderr)
        //     .try_init()?;

        let file =
            "/home/seth/.local/share/Steam/steamapps/common/Fallout New Vegas/Data/FalloutNV.esm";

        let data = fs::read(file)?;
        let mut cursor = Cursor::new(&data);
        let m = MainFile::read_options(
            &mut cursor,
            binrw::Endian::Little,
            &ParseOptions {
                ignore_entry: |_| false,
            },
        )?;

        let mut everything = Vec::new();
        fn insert<'a>(
            thems: impl Iterator<Item = &'a PluginEntry>,
            all: &mut Vec<(Ident4, &'a Record)>,
        ) {
            for them in thems {
                match them {
                    PluginEntry::Ignored(_) => (),
                    PluginEntry::Group { label, contents } => insert(contents.entries.iter(), all),
                    PluginEntry::Record { label, contents } => {
                        all.push((*label, contents));
                    }
                }
            }
        }
        insert(m.top_level_entries.iter(), &mut everything);

        for (n, e) in &everything {
            if let Some(id) = e.fields.common.editor_id.as_deref() {
                if id.to_lowercase().contains("boone") {
                    println!("[{n}]: {id}");
                }
            }
        }

        // let mut tle: Vec<(&GroupLabel, usize, &PluginEntry)> = m
        //     .top_level_entries
        //     .iter()
        //     .flat_map(|entry| match entry {
        //         crate::format::PluginEntry::Group { label, contents } => {
        //             let Some((first, _)) = contents.entries.split_first() else {
        //                 return None;
        //             };
        //             Some((label, contents.entries.len(), first))
        //         }
        //         _ => None,
        //     })
        //     .collect();

        // tle.sort_unstable_by_key(|&(_, len, _)| len as i64);

        // panic!("{tle:#?}");

        Ok(())
    }
}
