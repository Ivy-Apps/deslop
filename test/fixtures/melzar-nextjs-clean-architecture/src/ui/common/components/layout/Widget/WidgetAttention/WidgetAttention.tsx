import React from 'react';

import { Widget } from 'ui/common/components/layout/Widget/Widget';

import styles from 'ui/common/components/layout/Widget/WidgetAttention/widgetAttention.module.scss';

type Props = { children?: React.ReactNode };

export const WidgetAttention = ({ children }: Props) => (
  <Widget className={styles.widgetAttention}>{children}</Widget>
);
