import classNames from 'classnames';
import "./Button.scss";
import React from 'react';

type ButtonProps = React.HTMLAttributes<HTMLDivElement>

const Button: React.FC<ButtonProps> = ({ className = '', children, ...props }) => {
  return (
    <div className={classNames('button', className)} {...props}>
      {children}
    </div>
  )
};

export default Button