import React from 'react';

import { Widget } from 'ui/common/components/layout/Widget/Widget';

import styles from 'ui/common/components/layout/Widget/WidgetNew/widgetNew.module.scss';

type Props = { children?: React.ReactNode };

export const WidgetNew = ({ children }: Props) => (
  <Widget className={styles.widgetNew}>{children}</Widget>
);
