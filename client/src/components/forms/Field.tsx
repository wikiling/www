import React, { useState } from 'react';
import './Field.scss';
import classNames from 'classnames';
import { FieldError, useFormContext } from 'react-hook-form';
import useTwoClicks from 'hooks/useTwoClicks';

type FieldProps = React.DetailedHTMLProps<React.InputHTMLAttributes<HTMLInputElement>, HTMLInputElement> & {
  name: string
  initialValue?: string
  initialWidth?: number
  matchTextWidth?: boolean
  focusOnDblClick?: boolean
  noFocus?: boolean
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
  focusOnDblClick = false,
}) => {
  const {
    register,
    formState: { errors },
    setFocus
  } = useFormContext();
  const error = errors[name];
  const [textWidth, setTextWidth] = useState<number>(initialValue.length > 0 ? initialValue.length : initialWidth);
  // this is for book-keeping, not for setting focus
  const [isFocused, setIsFocused] = useState<boolean>(false);
  const handleFocus = () => setIsFocused(true);
  const handleBlur = () => setIsFocused(false);

  const handleChange = (e: React.ChangeEvent<HTMLInputElement>) => {
    setTextWidth(e.target.value.length);
    onChange && onChange(e);
  };

  const handleInputClick = useTwoClicks<HTMLInputElement>({
    onSingleClick: (e) => {
      console.log('single click', e)
      // if (!isFocused) {
      //   focusOnDblClick && e.preventDefault();
      // }
    },
    onDoubleClick: () => {
      console.log('double click')
      focusOnDblClick && setFocus('content')
    }
  });

  return (
    <div className={classNames('field', className, { 'field--invalid': !!error })}>
      <input
        onClick={handleInputClick}
        onFocus={handleFocus}
        style={{
          width: matchTextWidth ? `${textWidth}ch` : undefined,
          // pointerEvents: noFocus ? "none" : undefined,
          ...style
        }}
        spellCheck={spellCheck}
        autoComplete="off"
        placeholder={placeholder}
        {...register(name, { onChange: handleChange, onBlur: handleBlur })}
      />
    </div>
  );
};

export default Field