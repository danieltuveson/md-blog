# md-blog
A simple application for turning markdown files into blog posts.

If you'd like to steal the code:
- For the markdown parser, look at "Markdown.hs" in the src directory.
- For converting the AST of markdown to html, look at "HTML.hs" in the src directory.

Parsing of markdown is done using the parsec library. Converting to html is done using blaze-html. The backend is just wai + warp.