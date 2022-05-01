import classNames from 'classnames';
import styles from "./Button.scss";
import React, { useState } from 'react';
import ClipLoader from "react-spinners/ClipLoader";

type ButtonProps = React.HTMLAttributes<HTMLDivElement> & {
  trans?: boolean
  active?: boolean
  loading?: boolean
  onClick?: (e: React.MouseEvent<HTMLDivElement>) => any
}

const Button: React.FC<ButtonProps> = ({
  className = '',
  trans = true,
  active = false,
  loading,
  children,
  onClick,
  ...props
}) => {
  const [clickHandlerIsLoading, setClickHandlerIsLoading] = useState<boolean>(false);

  const handleClick = async (e: React.MouseEvent<HTMLDivElement>) => {
    if (!onClick) return;

    setClickHandlerIsLoading(true);
    await onClick(e);
    setClickHandlerIsLoading(false);
  }

  return (
    <div onClick={handleClick} className={classNames(
      'button',
      className,
      {
        'button--trans': trans,
        'button--active': active
      }
    )} {...props}>
      {clickHandlerIsLoading || loading ? <ClipLoader size={15} color={styles.borderColor}/> : children}
    </div>
  )
};

export default Button