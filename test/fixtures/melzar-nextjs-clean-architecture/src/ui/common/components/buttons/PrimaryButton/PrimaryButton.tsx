import React from 'react';

import { Button } from 'ui/common/components/buttons/Button/Button';

import styles from 'ui/common/components/buttons/PrimaryButton/primaryButton.module.scss';

type Props = {
  className?: string;
  children: React.ReactNode;
  onClick?: () => void;
  type: 'submit' | 'reset' | 'button' | undefined;
  testId?: string;
};
export const PrimaryButton = ({
  children,
  onClick,
  type = 'button',
  className,
  testId,
}: Props) => {
  return (
    <Button
      className={`${styles.primaryButton} ${className}`}
      type={type}
      onClick={onClick}
      testId={testId}
    >
      {children}
    </Button>
  );
};
