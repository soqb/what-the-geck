use std::fmt;

use enumflags2::BitFlags;
use miette::SourceCode;

pub struct MietteWrapper<'a, D: ?Sized> {
    inner: &'a D,
    source: &'a dyn SourceCode,
    severity: miette::Severity,
}

impl<'a, D: miette::Diagnostic + ?Sized> MietteWrapper<'a, D> {
    pub fn into_display(
        self,
        report_handler: &'a impl miette::ReportHandler,
    ) -> impl fmt::Display + 'a {
        struct Printer<'a, D: ?Sized, H> {
            error: MietteWrapper<'a, D>,
            handler: &'a H,
        }

        impl<'a, D: miette::Diagnostic + ?Sized, H: miette::ReportHandler> fmt::Display
            for Printer<'a, D, H>
        {
            fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
                self.handler.debug(&self.error, f)
            }
        }

        Printer {
            error: self,
            handler: report_handler,
        }
    }
}

pub fn miettify<'a, D: Diagnostic + ?Sized>(
    error: &'a D,
    source: &'a dyn SourceCode,
    filter: DiagnosticFilter,
) -> MietteWrapper<'a, D> {
    let severity = match error.kind() {
        DiagnosticKind::Nothing => miette::Severity::Advice,
        x if !filter.is_failing(x) => miette::Severity::Warning,
        _ => miette::Severity::Error,
    };
    MietteWrapper {
        inner: error,
        source,
        severity,
    }
}

impl<'a, T: miette::Diagnostic + ?Sized> fmt::Debug for MietteWrapper<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Debug::fmt(&self.inner, f)
    }
}

impl<'a, T: miette::Diagnostic + ?Sized> fmt::Display for MietteWrapper<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        fmt::Display::fmt(&self.inner, f)
    }
}

impl<'a, T: miette::Diagnostic + ?Sized> std::error::Error for MietteWrapper<'a, T> {
    fn source(&self) -> Option<&(dyn std::error::Error + 'static)> {
        std::error::Error::source(&self.inner)
    }
}

impl<'b, T: miette::Diagnostic + ?Sized> miette::Diagnostic for MietteWrapper<'b, T> {
    fn code<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.inner.code()
    }

    fn severity(&self) -> Option<miette::Severity> {
        Some(self.severity)
    }

    fn help<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.inner.help()
    }

    fn url<'a>(&'a self) -> Option<Box<dyn fmt::Display + 'a>> {
        self.inner.url()
    }

    fn source_code(&self) -> Option<&dyn SourceCode> {
        Some(self.source)
    }

    fn labels(&self) -> Option<Box<dyn Iterator<Item = miette::LabeledSpan> + '_>> {
        self.inner.labels()
    }

    fn related<'a>(&'a self) -> Option<Box<dyn Iterator<Item = &'a dyn miette::Diagnostic> + 'a>> {
        self.inner.related()
    }

    fn diagnostic_source(&self) -> Option<&dyn miette::Diagnostic> {
        self.inner.diagnostic_source()
    }
}

#[enumflags2::bitflags]
#[repr(u8)]
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord, Clone, Copy)]
pub enum Warning {
    BadPractice,
    Superfluous,
    UnspecifiedBehavior,
    SeemsVeryWrong,
}

/// The kind of a diagnostic.
#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum DiagnosticKind {
    /// Something other than an error.
    Nothing,
    /// Not an error, but something important to note.
    Warning(Warning),
    /// A syntax error.
    Syntax,
    /// An error indicating syntax could not be compiled.
    CompileFail,
    /// A wholly unrecoverable error or an error in correctness.
    Critical,
}

#[derive(PartialEq, Debug, Clone, Copy)]
pub struct DiagnosticFilter {
    pub lets_through: DiagnosticKind,
    pub suppressed_warnings: BitFlags<Warning>,
}

impl DiagnosticFilter {
    pub const NO_WARNINGS: BitFlags<Warning> = BitFlags::EMPTY;
    pub const ALL_WARNINGS: BitFlags<Warning> = BitFlags::ALL;

    pub fn is_failing(&self, diagnostic: DiagnosticKind) -> bool {
        fn priority(kind: &DiagnosticKind) -> u8 {
            match kind {
                DiagnosticKind::Nothing => 0,
                DiagnosticKind::Warning(_) => 1,
                DiagnosticKind::Syntax => 2,
                DiagnosticKind::CompileFail => 3,
                DiagnosticKind::Critical => 4,
            }
        }

        priority(&diagnostic) > priority(&self.lets_through)
    }

    pub fn is_suppressed(&self, diagnostic: DiagnosticKind) -> bool {
        !self.is_failing(diagnostic)
            && if let DiagnosticKind::Warning(warning) = diagnostic {
                self.suppressed_warnings.contains(warning)
            } else {
                false
            }
    }
}

impl Default for DiagnosticFilter {
    fn default() -> Self {
        Self {
            lets_through: DiagnosticKind::Critical,
            suppressed_warnings: DiagnosticFilter::ALL_WARNINGS,
        }
    }
}

pub trait CodeAction {
    fn title(&self) -> String;
}

pub type CodeActionIter<'a> = Box<dyn Iterator<Item = &'a (dyn CodeAction + 'a)> + 'a>;

pub trait Diagnostic: miette::Diagnostic + Send + Sync {
    fn kind(&self) -> DiagnosticKind;
    fn code_actions(&self) -> CodeActionIter<'_> {
        Box::new(std::iter::empty())
    }
}

pub struct ListFmt<'a, T> {
    list: &'a [T],
    sep: &'a str,
}

impl<'a, T: fmt::Display> ListFmt<'a, T> {
    pub fn new(list: &'a [T], sep: &'a str) -> Self {
        Self { list, sep }
    }
}

impl<'a, T: fmt::Display> fmt::Display for ListFmt<'a, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let Some((last, rest)) = self.list.split_last() else {
            return Ok(());
        };

        for element in rest {
            write!(f, "{element}{sep}", sep = self.sep)?;
        }

        write!(f, "{last}")
    }
}

pub type Result<T, E = Box<dyn Diagnostic>> = core::result::Result<T, E>;
