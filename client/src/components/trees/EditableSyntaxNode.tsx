import Field from 'components/forms/Field';
import Form from 'components/forms/Form';
import { forwardRef, useEffect, useState } from 'react'
import { useForm } from "react-hook-form";
import { getTextDimensions } from 'utils/document';
import { EditableSyntaxNodeValues, CoordinatedTreeNode } from "./types";

type EditableSyntaxNodeProps = {
  node: CoordinatedTreeNode
  onSubmit: (values: EditableSyntaxNodeValues) => void
}

// FIXME: centralize the default font size
const DEFAULT_FONT_SIZE = 16;
const DEFAULT_EMPTY_WIDTH = 5;
const chToPx = (ch: number) => (ch / 2) * DEFAULT_FONT_SIZE;

const EditableSyntaxNode = forwardRef<
  SVGGElement, EditableSyntaxNodeProps
>(({ node, onSubmit }, forwardedRef) => {
  const fieldName = 'label';
  const initialValue = node.data.label;
  const initialValueDims = getTextDimensions(initialValue);

  const [width, setWidth] = useState<number>(initialValue.length === 0 ? DEFAULT_EMPTY_WIDTH : initialValue.length); // ch
  const [height] = useState<number>(1.25 * initialValueDims.height); // px
  const [x, setX] = useState<number>(node.x - width / 2);
  const [y] = useState<number>(node.y);

  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const length = e.target.value.length;

    setWidth(length > 0 ? length : DEFAULT_EMPTY_WIDTH);
    setX(node.x - chToPx(length) / 2);
  };

  return (
    <g ref={forwardedRef} className="node node-editable">
      <foreignObject
        width={`${width}ch`}
        height={height}
        x={x}
        y={y}
      >
        <Form<EditableSyntaxNodeValues>
          onSubmit={onSubmit}
          options={{
            defaultValues: {
              [fieldName]: initialValue,
              id: node.data.id
            }
          }}
        >
          {() =>
            <Field
              autoFocus
              name={fieldName}
              initialValue={initialValue}
              onChange={handleChange}
            />
          }
        </Form>
      </foreignObject>
    </g>
  );
});

export default EditableSyntaxNode