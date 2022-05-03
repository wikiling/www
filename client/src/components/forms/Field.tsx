import React, { useState } from 'react';
import './Field.scss';
import classNames from 'classnames';
import { FieldError } from 'react-hook-form';

type FieldProps = React.DetailedHTMLProps<React.InputHTMLAttributes<HTMLInputElement>, HTMLInputElement> & {
  initialValue?: string
  initialWidth?: number
  error?: FieldError
  matchTextWidth?: boolean
}

const Field = React.forwardRef<HTMLInputElement, FieldProps>(({
  initialValue = '',
  initialWidth = 1,
  error,
  className,
  onChange,
  style,
  spellCheck = false,
  matchTextWidth = false,
  ...props
}, forwardedRef) => {
  const [textWidth, setTextWidth] = useState<number>(initialValue.length > 0 ? initialValue.length : initialWidth);

  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setTextWidth(e.target.value.length);
    onChange && onChange(e);
  };

  const styles = {
    width: matchTextWidth ? `${textWidth}ch` : undefined,
    ...style
  };
  
  return (
    <div className={classNames('field', className, { 'field--invalid': !!error })}>
      <input
        ref={forwardedRef}
        onChange={handleChange}
        style={styles}
        spellCheck={spellCheck}
        autoComplete="off"
        {...props}
      />
    </div>
  );
});

export default Field