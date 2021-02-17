# How I Built This Site Part I: The Parser
###### *2020-09-15*

*Sigh*. Okay so I've spent a couple weeks fiddling around with this site, and I've reached that point where I'm kind of tired of looking at this code. But building this took some work! And I've learned a little more about some pretty popular and useful Haskell libraries, which I think is worth sharing. As I go through some of the code, I'll try to explain what it's doing, and provide background for anything that might be confusing to someone not familiar with Haskell.

If you want to know what I was thinking when I started building this out, check out my [previous post](http://danieltuveson.com/posts/2020-09-02-hello-world.md)

## The Parser
One of my goals with this site was to write a simple parser that could take in some markdown and programmatically generate some HTML. Despite being somewhat upopular, Haskell actually has a decent number of parsing libraries. Probably not too surprising given that it's commonly used for writing compilers. Not to mention that maybe the most widely used tool writen in Haskell is [Pandoc](https://github.com/jgm/pandoc), which is used for converting text-like files from one format to another (a problem that I imagine requires writing lots of parsing). I digress! I wound up picking Parsec, which isn't the most efficient, but is incredibly easy to work with and fast enough for my purposes.

#### A Brief Haskell / Parsec lesson
It might be hard to describe the code I'm working with if I don't give you a quick crash course in Haskell datatypes. Most popular languages are object-oriented, and if you want to define a group of data, you do so with an object. Something like: 

```java
class Markdown {
    List<String> somePrivateState = new ArrayList[20]
    int someInt = 5
    ... getters, setters, other methods ...
}; 
```

```haskell
-- Markdown.hs
data Markdown
  = MDText [MDText]
  | Header MDHeader [MDText]
  | CodeBlock Language String 
  | List MDList 
  deriving(Show, Eq)
```

The snippet above describes at a high level how I want to think about the contents of a markdown file. Markdown can either be a header, a block of code, a list (ordered or unordered), or just some text. For those not familiar with Haskell, this is defining a _sum type_ sometimes known as a tagged union, depending on the language you're working with. This lets one define a list of possible things a `Markdown` type could be, each with a unique _contructor_ (e.g. `CodeBlock`). It's particularly useful, because it allows one type to be many different things, but unlike a dynamically typed languge, it will complain at compile time if we try to use a `Markdown` variable without specifying what to do for each possible constructor. Here's how you would parse that using Parsec:

```haskell
parseMarkdown :: Parser [Markdown]
parseMarkdown = manyTill parseMD checkEOF
  where parseMD = parseHeader 
              <|> parseList 
              <|> codeBlock 
              <|> parseMDLineWithNewline
```

Pretty straightforward. `parseMD` is a parser that tries to parse a header, and if that fails, will try to parse a list, etc... If all of those fail, it will just parse a line of text (with possible inline styles). `manyTill parseMD checkEOF` is saying that we want to keep trying to parse the text as `Markdown` until we hit the end of the file, and return the list of `Markdown` chunks.