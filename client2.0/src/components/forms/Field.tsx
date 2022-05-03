import React, { useState } from 'react';
import './Field.scss';
import classNames from 'classnames';


type FieldProps = React.HTMLAttributes<HTMLInputElement> & {
  initialValue: string
  initialWidth?: number
}

const Field = React.forwardRef<HTMLInputElement, FieldProps>(({
  initialValue,
  initialWidth = 1,
  className,
  onChange,
  style,
  spellCheck = false,
  ...props
}, ref) => {
  const [width, setWidth] = useState<number>(initialValue.length > 0 ? initialValue.length : initialWidth);

  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setWidth(e.target.value.length);
    onChange && onChange(e);
  };
  
  return (
    <input
      ref={ref}
      className={classNames('field', className)}
      onChange={handleChange}
      style={{ width: `${width}ch`, ...style }}
      spellCheck={spellCheck}
      autoComplete="off"
      {...props}
    />
  );
});

export default Field