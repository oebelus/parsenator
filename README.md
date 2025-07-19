# My parser combinator Library

## 1. Basic Combinators

### Specific Matchers

They match a specific input:

- `char` – Parses a single specific character;
- `literal` – Parses a specific sequence of characters;
- `starts_with` - Check if an input starts with a given prefix;
- `ends_with` - Check if an input ends with a given prefix;

### General Matchers

They succeed on any valid input, rather than matching an exact expected value:

- `digit` – Parses a single digit (0-9);
- `number` – Parses many digits;
- `letter` – Parses a single letter (a-z, A-Z);
- `word` - Parses many letters;
- `any_char` – Parses any single character;
- `alpha_num` – Parses a single alphanumeric character (letter or digit);
- `alpha_num_word` - Parses a sequence of alphanumeric characters;
- `whitespace` – Parses a single whitespace character (space, tab, newline);
- `spaces` – Parses zero or more whitespace characters;
- `end_of_input` – Succeeds only if the end of the input is reached.

## 2. Logical Combinators

- `or` – Tries the first parser, and if it fails, tries the second (alternative);
- `and` – Runs two parsers in sequence and combines their results;
- `zero_or_more` – Parses zero or more occurrences of a given parser;
- `one_or_more` – Parses one or more occurrences of a given parser;
- `optional` – Parses zero or one occurrence of a given parser;
- `sep_by` – Parses a sequence separated by a delimiter (e.g., `1,2,3` to `[1, 2, 3]`);
- `between` – Parsers something between two other parsers (e.g., brackets `(x)` to `x`);

## 3. Transformation

- `map` – Applies a function to the result of a parser;
- `seq` – Chains parsers;
- `choice` - Tries each parser in order until one succeeds;
- `left` - Runs two parsers in sequence and returns the result of the first parser;
- `right` - Runs two parsers in sequence and returns the result of the right parser;
- `look_ahead` – Checks if a parser matches without consuming input;
- `not_followed_by` – The opposite of `look_ahead`, succeeds only if the given parser fails.

## 4. Error Handling

- `fail` – Always fails with a given error message:
  - For custom validation logic (e.g., "number must be positive");
  - As a placeholder for unimplemented parsers.
