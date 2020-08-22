# Howdy world

I've been working with Haskell for a while now, on and off in my spare time, but I haven't gotten the chance to do anything really practical with it. So far, I wrote a sudoku solver, a simple programming language for performing basic calculations, and *most of* a command line chess application (along with about 10 half finished and abandoned projects). So like all imagination-lacking programmers do eventually do, I decided to make a blog backend with it. 

Since I'm not too experienced with web development (in Haskell or anything other language, really), I thought the most fun thing to do would be to make a simple blog backend written mostly from scratch. I've tried a few Haskell backend frameworks in the past, but most of them either didn't work, were poorly documented, or were overkill for what I want.

## The Specifications:
All I want is to be able to write markdown and have it show up as a blog post. For that, I'll need two things:
- Something that can convert markdown to HTML
- Something that can serve that HTML to a reader

## Libraries: 
Since I don't want things to get too complicated, I'm going to go with a couple of simple, fairly well documented libraries.
- WAI / Warp for the server
- Blaze HTML for rendering HTML 
- Parsec for converting the markdown to HTML
- Stack for building / managing dependencies


## Step 1: Building The Markdown Parser
I'm going to start by building out the markdown parser. With Parsec, building parsers is pretty straightforward. Even if you need to build a moderately complex parser or compiiler, I would strongly consider using Parsec. Once you've defined the datatypes, you basically write in plain english how you want to parse the code. It's pretty refeshing, especially if you are like me and find moderately complex regex to be completely unreadable.

I'll start by just building out the most basic functionality, so 
- Bolding / italics / links
- Lists
- Headers
- Code

I'll probably add in functionality for images, tables, and stuff like that later, but for now I just want something to be able to convert this post to HTML and serve it. 