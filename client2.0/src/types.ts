import { HierarchyNode } from "d3-hierarchy"

declare module "d3-hierarchy" {
  interface HierarchyLink<Datum> {
    id: string
  }

  interface HierarchyNode<Datum> {
    links(): Array<HierarchyLink<Datum>>;
  }

}

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

export type SyntaxTreeID = string

export type IncomingSyntaxTree = {
  id: SyntaxTreeID
  pos?: string
  token?: string
  children?: IncomingSyntaxTree[]
}

export type NormalizedSyntaxTree = {
  id: SyntaxTreeID
  text: string
  children?: NormalizedSyntaxTree[]
}

export type SyntaxTree = HierarchyNode<NormalizedSyntaxTree>

export type IncomingSentence = {
  id: ID
  description: string
  content: string
  // parse_type: ParseTypes.CONSTITUENCY
  has_punctuation: boolean
  syntax_tree: IncomingSyntaxTree
}

export type Sentence = Omit<IncomingSentence, 'syntax_tree'> & {
  syntaxTree: SyntaxTree
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
