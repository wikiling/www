import { HierarchyNode } from "d3-hierarchy"

export type toEnumType<EnumType> = EnumType[keyof EnumType]

export const ParseTypes = {
  CONSTITUENCY: 'CONSTITUENCY'
} as const

export type ParseType = toEnumType<typeof ParseTypes>

export type ID = number

export type Author = {
  id: ID
  full_name: string
}

export type SyntaxTreeID = string;
export type SemanticTreeID = string;

export type SyntaxTree = {
  id: SyntaxTreeID
  pos?: string
  token?: string
  children?: [SyntaxTree]
}

export type SemanticTree = {
  id: SemanticTreeID
  formula: string
  children?: [SemanticTree]
}

export type CoordinatedSyntaxTree = HierarchyNode<SyntaxTree>

export type IncomingSentence = {
  id: ID
  description: string
  content: string
  // parse_type: ParseTypes.CONSTITUENCY
  has_punctuation: boolean
  syntax_tree: SyntaxTree
}

export type Sentence = Omit<IncomingSentence, 'syntax_tree'> & {
  syntax_tree: CoordinatedSyntaxTree
}

export type IncomingText = {
  id: ID
  author_id: ID
  title: string
  content: string
  sentences: IncomingSentence[]
}

export type Text = Omit<IncomingText, 'sentences'> & {
  sentences: Sentence[]
}

export type AuthorTextRouteParams = {
  authorId: string
  textId: string
}