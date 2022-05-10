import { HierarchyNode } from "d3-hierarchy"

export type toEnumType<EnumType> = EnumType[keyof EnumType]

export type ID = number
export type UUID = string
export type Slug = string

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
  children?: SyntaxTree[]
}

export type SemanticTree = {
  id: SemanticTreeID
  formula: string
  children?: [SemanticTree]
}

export type CoordinatedSyntaxTree = HierarchyNode<SyntaxTree>

export type ExampleBase = {
  fragment_id: ID
  description: string
  content: string
  label: string
}

export type Example = ExampleBase & { id: ID }
export type TemporaryExample = ExampleBase & {
  temp_id: UUID
}

export type ExampleEditValues = Pick<ExampleBase, 'label' | 'content'>
export type ExampleCreateValues = ExampleBase

export type ConstituencyParse = {
  id: ID
  example_id: ID
  parse_string: string
  syntax_tree: SyntaxTree
}

export type ConstituencyParseNodeEditValues = {
  nodeId: SyntaxTreeID
  nodeText: string
}

export type ConstituencyParseEditValues = {
  parse_string: string
}

export type CoordinatedConstituencyParse = ConstituencyParse & {
  coordinated_syntax_tree: CoordinatedSyntaxTree
}

export type Fragment = {
  id: ID
  slug: Slug
  author: Author
  title: string
  examples: Example[]
}

export type FragmentDetailRouteParams = {
  fragmentSlug: Slug
}