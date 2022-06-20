import React, { useState } from 'react';
import './Field.scss';
import classNames from 'classnames';
import { useFormContext } from 'react-hook-form';

type FieldProps = React.DetailedHTMLProps<React.InputHTMLAttributes<HTMLInputElement>, HTMLInputElement> & {
  name: string
  initialValue?: string
  initialWidth?: number
  matchTextWidth?: boolean
}

const Field: React.FC<FieldProps> = ({
  name,
  initialValue = '',
  initialWidth = 1,
  className,
  onChange,
  style,
  placeholder,
  spellCheck = false,
  matchTextWidth = false,
  ...props
}) => {
  const ctx = useFormContext();
  const {
    register,
    formState: { errors },
  } = ctx;
  const error = errors[name];
  const [textWidth, setTextWidth] = useState<number>(initialValue.length > 0 ? initialValue.length : initialWidth);

  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setTextWidth(e.target.value.length);
    onChange && onChange(e);
  };

  return (
    <div
      className={classNames('field', className, { 'field--invalid': !!error })}
    >
      <input
        style={{
          width: matchTextWidth ? `${textWidth}ch` : undefined,
          ...style
        }}
        spellCheck={spellCheck}
        autoComplete="off"
        placeholder={placeholder}
        {...props}
        {...register(name, { onChange: handleChange })}
      />
    </div>
  );
};

export default Field;