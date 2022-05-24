import Field from 'components/forms/Field';
import { forwardRef, useEffect, useState } from 'react'
import { useForm } from "react-hook-form";
import { getTextDimensions } from 'utils/document';
import { EditableNodeValues, CoordinatedTreeNode } from "./types";

type EditableNodeProps = {
  node: CoordinatedTreeNode
  onSubmit: (values: EditableNodeValues) => void
}

// FIXME: centralize the default font size
const DEFAULT_FONT_SIZE = 16;
const DEFAULT_EMPTY_WIDTH = 5;
const chToPx = (ch: number) => (ch / 2) * DEFAULT_FONT_SIZE;

const EditableNode = forwardRef<
  SVGGElement, EditableNodeProps
>(({ node, onSubmit }, forwardedRef) => {
  const fieldName = 'label';
  const initialValue = node.data.label;
  const initialValueDims = getTextDimensions(initialValue);
  const {
    register,
    handleSubmit,
    setFocus
  } = useForm<EditableNodeValues>({
    defaultValues: {
      [fieldName]: initialValue,
      id: node.data.id
    }
  });

  const [width, setWidth] = useState<number>(initialValue.length === 0 ? DEFAULT_EMPTY_WIDTH : initialValue.length); // ch
  const [height] = useState<number>(1.25 * initialValueDims.height); // px
  const [x, setX] = useState<number>(node.x - width / 2);
  const [y] = useState<number>(node.y - initialValueDims.height / 2);

  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    const length = e.target.value.length;

    setWidth(length > 0 ? length : DEFAULT_EMPTY_WIDTH);
    setX(node.x - chToPx(length) / 2);
  };

  const registration = register(fieldName, { onChange: handleChange });

  useEffect(() => {
    setFocus(fieldName);
  }, [])

  return (
    <g ref={forwardedRef} className="node node-editable">
      <rect x={x} y={y} width={`${width}ch`} height={height} fill="white"/>
      <foreignObject
        width={`${width}ch`}
        height={height}
        x={x}
        y={y}
      >
        <form onSubmit={handleSubmit(onSubmit)}>
          <Field
            initialValue={initialValue}
            {...registration}
          />
        </form>
      </foreignObject>
    </g>
  );
});

export default EditableNode