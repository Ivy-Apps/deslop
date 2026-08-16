import React from 'react';

import { Widget } from 'ui/common/components/layout/Widget/Widget';

import styles from 'ui/common/components/layout/Widget/WidgetCompleted/widgetCompleted.module.scss';

type Props = { children?: React.ReactNode };

export const WidgetCompleted = ({ children }: Props) => (
  <Widget className={styles.widgetCompleted}>{children}</Widget>
);
