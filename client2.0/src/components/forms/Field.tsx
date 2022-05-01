import React from 'react';
import './Field.scss';
import classNames from 'classnames';

type FieldProps = React.HTMLAttributes<HTMLInputElement>

const Field = React.forwardRef<HTMLInputElement, FieldProps>(({ className, ...props }, ref) => {
  return (
    <input ref={ref} className={classNames('field', className)} {...props}/>
  );
});

export default Field