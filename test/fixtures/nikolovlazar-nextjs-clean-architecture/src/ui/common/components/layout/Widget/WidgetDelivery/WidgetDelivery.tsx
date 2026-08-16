import React from 'react';

import { Widget } from 'ui/common/components/layout/Widget/Widget';

import styles from 'ui/common/components/layout/Widget/WidgetDelivery/widgetDelivery.module.scss';

type Props = { children?: React.ReactNode };

export const WidgetDelivery = ({ children }: Props) => (
  <Widget className={styles.widgetDelivery}>{children}</Widget>
);
