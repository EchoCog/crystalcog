# Natural Language Processing (NLP) Module

This module provides basic natural language processing functionality for the CrystalCog OpenCog implementation. It includes text tokenization, processing, and integration with the AtomSpace for storing linguistic knowledge.

## Features

### Core Functionality

- **Text Tokenization**: Split text into words, sentences, and tokens with classification
- **Text Processing**: Normalization, stop word removal, stemming, and keyword extraction  
- **AtomSpace Integration**: Store linguistic knowledge as atoms and relationships
- **Semantic Relations**: Create and manage semantic relationships between words
- **Statistical Analysis**: Calculate text and linguistic complexity metrics

### Modules

#### `NLP::Tokenizer`
- Basic and advanced tokenization
- Token classification (words, numbers, punctuation)
- Sentence splitting
- N-gram extraction
- Token feature analysis

#### `NLP::TextProcessor`
- Text normalization and preprocessing
- Stop word removal (English)
- Simple stemming algorithm
- Term frequency calculation
- Keyword extraction
- Text statistics

#### `NLP::LinguisticAtoms`
- Word atom creation with part-of-speech tagging
- Sentence structure representation in AtomSpace
- Semantic relationship creation (synonyms, antonyms, hypernyms)
- Parse tree structures
- Linguistic complexity metrics

## Usage Examples

### Basic Text Processing

```crystal
require "./src/nlp/nlp"

# Initialize systems
CogUtil.initialize
AtomSpace.initialize
NLP.initialize

# Create an AtomSpace
atomspace = AtomSpace::AtomSpace.new

# Process text
text = "The quick brown fox jumps over the lazy dog."
atoms = NLP.process_text(text, atomspace)

puts "Created #{atoms.size} atoms from text processing"
```

### Tokenization

```crystal
# Basic tokenization
text = "Hello, world! This is a test."
tokens = NLP::Tokenizer.tokenize(text)
# => ["hello", "world", "this", "is", "a", "test"]

# Advanced tokenization with features
features = NLP::Tokenizer.tokenize_with_features(text)
features.each do |feature|
  puts "#{feature[:token]} (#{feature[:type]}) at position #{feature[:position]}"
end

# Sentence splitting
sentences = NLP::Tokenizer.split_sentences(text)
# => ["Hello, world", "This is a test"]
```

### Text Processing

```crystal
# Text normalization
normalized = NLP::TextProcessor.normalize_text("HELLO   World\t\n")
# => "hello world"

# Stop word removal
tokens = ["the", "quick", "brown", "fox"]
filtered = NLP::TextProcessor.remove_stop_words(tokens)
# => ["quick", "brown", "fox"]

# Keyword extraction
keywords = NLP::TextProcessor.extract_keywords(text, 5)
puts "Keywords: #{keywords.join(", ")}"

# Text statistics
stats = NLP::TextProcessor.get_text_stats(text)
puts "Words: #{stats["word_count"]}, Sentences: #{stats["sentence_count"]}"
```

### Linguistic Atoms

```crystal
# Create word atoms
word_atom = NLP::LinguisticAtoms.create_word_atom(atomspace, "cat", "noun")

# Create semantic relationships
relation = NLP::LinguisticAtoms.create_semantic_relation(
  atomspace, "dog", "animal", "isa", 0.9
)

# Create sentence structure
tokens = ["the", "cat", "sits"]
sentence_atoms = NLP::LinguisticAtoms.create_sentence_structure(atomspace, tokens)

# Get linguistic statistics
complexity = NLP::LinguisticAtoms.get_linguistic_complexity(atomspace)
puts "Linguistic complexity: #{complexity}"
```

## Command Line Interface

The NLP module includes a command-line interface for testing and demonstration:

```bash
# Run comprehensive demonstration
crystal run src/nlp/nlp_main.cr -- demo

# Tokenize text
crystal run src/nlp/nlp_main.cr -- tokenize "Hello, world!"

# Process text into AtomSpace
crystal run src/nlp/nlp_main.cr -- process "The cat sits on the mat."

# Analyze text statistics
crystal run src/nlp/nlp_main.cr -- stats "Natural language processing is fascinating."
```

## Integration with CrystalCog

The NLP module integrates seamlessly with other CrystalCog components:

- **AtomSpace**: Stores linguistic knowledge alongside other knowledge representations
- **PLN**: Can reason about linguistic relationships and semantic knowledge
- **URE**: Can apply rules to linguistic structures
- **CogUtil**: Uses logging and configuration systems

## Testing

Comprehensive test suite covers all functionality:

```bash
# Run all NLP tests
crystal spec spec/nlp/

# Run specific test files
crystal spec spec/nlp/tokenizer_spec.cr
crystal spec spec/nlp/text_processor_spec.cr
crystal spec spec/nlp/linguistic_atoms_spec.cr
```

## Architecture

The NLP module follows the established CrystalCog patterns:

- **Modular Design**: Separate modules for different functionality
- **Exception Handling**: Custom exception classes for error handling
- **AtomSpace Integration**: Native integration with knowledge representation
- **Crystal Idioms**: Uses Crystal language features and patterns
- **Comprehensive Testing**: Full test coverage with realistic examples

## Supported Features

### Text Processing
- ✅ Tokenization with classification
- ✅ Text normalization and preprocessing
- ✅ Stop word removal (English)
- ✅ Basic stemming
- ✅ N-gram extraction
- ✅ Keyword extraction
- ✅ Term frequency analysis

### Linguistic Representation
- ✅ Word atoms with part-of-speech
- ✅ Sentence structure representation
- ✅ Semantic relationships (synonyms, antonyms, hypernyms)
- ✅ Sequential word ordering
- ✅ Parse tree structures
- ✅ Linguistic complexity metrics

### Integration Features
- ✅ AtomSpace native integration
- ✅ Compatible with PLN reasoning
- ✅ Compatible with URE rule engine
- ✅ CogUtil logging integration
- ✅ Error handling and validation

## Future Enhancements

Potential extensions for the NLP module:

- **Parser Integration**: Interface with external parsers (RelEx, spaCy)
- **Advanced Stemming**: Porter stemmer or lemmatization
- **Named Entity Recognition**: Identify and classify named entities
- **Dependency Parsing**: Syntactic dependency relationships
- **Semantic Role Labeling**: Predicate-argument structures
- **Multi-language Support**: Beyond English tokenization and processing
- **Machine Learning Integration**: Neural language models
- **Discourse Processing**: Anaphora resolution and discourse structure

## Performance Notes

The NLP module is designed for:

- **Memory Efficiency**: Minimal memory footprint for text processing
- **Speed**: Fast tokenization and text processing
- **Scalability**: Handles varying text sizes efficiently
- **Thread Safety**: Compatible with concurrent AtomSpace operations

## Dependencies

- **CogUtil**: Logging and configuration
- **AtomSpace**: Knowledge representation and storage
- **Crystal Standard Library**: String processing and collections

This implementation provides a solid foundation for natural language processing in CrystalCog while maintaining compatibility with the existing OpenCog architecture.