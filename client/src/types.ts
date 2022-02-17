export type ID = number

export type Author = {
  id: ID
  full_name: string
}

export type IncomingSyntaxTree = {
  pos?: string
  token?: string
  children?: SyntaxTree[]
}

export type SyntaxTree = {
  text: string
  children?: SyntaxTree[]
}

export type IncomingText = {
  id: ID
  author_id: ID
  title: string
  content: string
  syntax_trees: IncomingSyntaxTree[]
}

export type Text = Omit<IncomingText, 'syntax_trees'> & {
  syntax_trees: SyntaxTree[]
}
