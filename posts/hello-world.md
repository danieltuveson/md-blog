# Howdy world
I've been working with Haskell for a while now, on and off in my spare time, but I haven't gotten the chance to do anything really practical with it. So far, I wrote a sudoku solver, a simple programming language for performing basic calculations, and *most of* a command line chess application (along with about 10 half finished and abandoned projects). So like all imagination-lacking programmers do eventually do, I decided to make a blog backend with it. 

Since I'm not too experienced with web development (in Haskell or anything other language, really), I thought the simplest thing to do would be to write it mostly from scratch, and just have it serve static pages: nothing fancy. I've tried a few Haskell backend frameworks in the past, but most of them either didn't build on my machine, were poorly documented, or were overkill for what I want. So I've decided to just use WAI / Warp, which are the core libraries used by most Haskell backend frameworks. Even if I decide I want to switch over to one of those frameworks down the line, building some knowledge of WAI / Warp will probably be an asset.

## The Specifications:
All I want is to be able to write markdown and have it show up as a blog post. For that, I'll need two things:
- Something that can convert markdown to HTML
- Something that can serve that HTML to a reader

## Libraries: 
Since I don't want things to get too complicated, I'm going to go with a couple of simple, fairly well documented libraries.
- Parsec for converting the markdown to HTML
- Blaze HTML for rendering HTML 
- WAI / Warp for the server
- Stack for building / managing dependencies

## Step 1: Building The Markdown Parser
I'm going to start by building out the markdown parser. With Parsec, building parsers is pretty straightforward. If you need to build a moderately complex parser or compiler where performance is not the most important thing in the world, I would strongly consider using Parsec (and Haskell still has many great options that are similar enough where you could migrate if performance becomes important). Once you've defined the datatypes, you basically write in plain english how you want to parse the code. It's pretty readable and parsers compose quite easily. Want to parse a newline? Write `newline`. Want to parse some letter in "abc"? Write `oneOf "abc"`. Want to do some lookahead using a specific parser? Use `lookAhead` with your parser. It's a joy to work with, and the library is practically self-documenting. 

I'll start by just building out the most basic functionality, so 
- Inline styles: **Bolding** / *italics* / ~~strikethroughs~~ / `inline code` / [links](https://daniel-tuveson.herokuapp.com/)
- Lists
- Headers
- Code blocks

I'll add in functionality for images, tables, and stuff like that later, but for now I just want something to be able to convert this post to HTML and serve it.

## Step 2: Building the backend
This step is going to be pretty simple, since I literally just want to serve a post sitting in a folder. What I'll do is have the program convert all markdown files to blaze-html bytestrings, then serve those as people request them. 

In the future I may convert this site to some other backend, but for now this site exists as a way for me to play around with the more low level features of a web backend, so I'm going to continue using plain WAI. 


