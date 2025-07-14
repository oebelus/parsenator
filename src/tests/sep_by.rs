use crate::parser::{digits, sep_by, Parser};

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn test_sep_by_basic() {
        let parser = sep_by(digits(), ";;");
        assert_eq!(
            parser.parse("1;;2;;3"),
            Ok(("", vec!["1".to_string(), "2".to_string(), "3".to_string()]))
        );
    }

    #[test]
    fn test_sep_by_with_spaces() {
        let parser = sep_by(digits(), ";;");
        assert_eq!(
            parser.parse("1;; 2 ;; 3"),
            Ok(("", vec!["1".to_string(), "2".to_string(), "3".to_string()]))
        );
    }

    #[test]
    fn test_sep_by_trailing_delimiter() {
        let parser = sep_by(digits(), ";;");
        assert_eq!(
            parser.parse("1;;2;;3;;"),
            Ok((
                ";;",
                vec!["1".to_string(), "2".to_string(), "3".to_string()]
            ))
        );
    }

    #[test]
    fn test_sep_by_leading_delimiter() {
        let parser = sep_by(digits(), ";;");
        assert!(parser.parse(";;1;;2").is_err());
    }

    #[test]
    fn test_sep_by_empty_input() {
        let parser = sep_by(digits(), ";;");
        assert!(parser.parse("").is_err());
    }
    #[test]
    fn test_sep_by_single_item() {
        let parser = sep_by(digits(), ";;");
        assert_eq!(parser.parse("42"), Ok(("", vec!["42".to_string()])));
    }
    #[test]
    fn test_sep_by_wrong_delimiter() {
        let parser = sep_by(digits(), ";;");
        assert_eq!(
            parser.parse("1::2;;3"),
            Ok(("::2;;3", vec!["1".to_string()])) // Should stop at first invalid delimiter
        );
    }
    #[test]
    fn test_sep_by_mixed_whitespace() {
        let parser = sep_by(digits(), ";;");
        assert_eq!(
            parser.parse("1;;\t2;;\n3"),
            Ok(("", vec!["1".to_string(), "2".to_string(), "3".to_string()]))
        );
    }
    #[test]
    fn test_sep_by_multiple_spaces() {
        let parser = sep_by(digits(), ";;");
        assert_eq!(
            parser.parse("1;;     2;;3"),
            Ok(("", vec!["1".to_string(), "2".to_string(), "3".to_string()]))
        );
    }
    #[test]
    fn test_sep_by_partial_delimiter() {
        let parser = sep_by(digits(), ";;");
        assert_eq!(
            parser.parse("1;2;;3"),
            Ok((";2;;3", vec!["1".to_string()])) // Should stop at incomplete delimiter
        );
    }
}
