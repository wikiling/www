import classNames from 'classnames';
import styles from "./Button.scss";
import React, { useState } from 'react';
import ClipLoader from "react-spinners/ClipLoader";

type Mode = 'trans' | 'clear'

type ButtonProps = React.HTMLAttributes<HTMLDivElement> & {
  mode?: Mode
  active?: boolean
  loading?: boolean
  onClick: (e: React.MouseEvent<HTMLDivElement>) => any
}

const Button: React.FC<ButtonProps> = ({
  className = '',
  mode = 'trans',
  active = false,
  loading,
  children,
  onClick,
  ...props
}) => {
  const [clickHandlerIsExecuting, setClickHandlerIsExecuting] = useState<boolean>(false);

  const handleClick = async (e: React.MouseEvent<HTMLDivElement>) => {
    setClickHandlerIsExecuting(true);
    await onClick(e);
    setClickHandlerIsExecuting(false);
  }

  return (
    <div onClick={handleClick} className={classNames(
      'button',
      `button--${mode}`,
      className,
      {
        'button--active': active
      }
    )} {...props}>
      {clickHandlerIsExecuting || loading ? <ClipLoader size={15} color={styles.borderColor}/> : children}
    </div>
  )
};

export default Button