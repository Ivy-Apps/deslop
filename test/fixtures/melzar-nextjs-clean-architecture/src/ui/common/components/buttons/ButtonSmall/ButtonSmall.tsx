import React from 'react';

import { Button } from 'ui/common/components/buttons/Button/Button';

import styles from 'ui/common/components/buttons/ButtonSmall/buttonSmall.module.scss';

type Props = {
  className?: string;
  children: React.ReactNode;
  onClick?: () => void;
  type: 'submit' | 'reset' | 'button' | undefined;
};
export const ButtonSmall = ({
  children,
  onClick,
  type = 'button',
  className,
}: Props) => (
  <Button
    className={`${styles.buttonSmall} ${className}`}
    type={type}
    onClick={onClick}
  >
    {children}
  </Button>
);
