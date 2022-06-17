import classNames from 'classnames';
import styles from "./Button.scss";
import React, { useState } from 'react';
import ClipLoader from "react-spinners/ClipLoader";
import useLoadWhile from 'hooks/useLoadWhile';

type Mode = 'trans' | 'clear' | 'menu'

type ButtonProps = React.HTMLAttributes<HTMLDivElement> & {
  mode?: Mode
  active?: boolean
  isLoading?: boolean
  onClick: (e: React.MouseEvent<HTMLDivElement>) => any
}

const Button: React.FC<ButtonProps> = ({
  className = '',
  mode = 'trans',
  active = false,
  isLoading,
  children,
  onClick,
  ...props
}) => {
  const { isLoading: isClickHandlerExecuting, loadWhile: handleClickWhile } = useLoadWhile();

  const handleClick = async (e: React.MouseEvent<HTMLDivElement>) => {
    if (isLoading) return e.stopPropagation();

    handleClickWhile(async () => await onClick(e));
  }

  return (
    <div onClick={handleClick} className={classNames(
      'button',
      `button--${mode}`,
      className,
      {
        'button--active': active,
      }
    )} {...props}>
      {isClickHandlerExecuting || isLoading ? <ClipLoader size={15} color={styles.borderColor}/> : children}
    </div>
  )
};

export default Button