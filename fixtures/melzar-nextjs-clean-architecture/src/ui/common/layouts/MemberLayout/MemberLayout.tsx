import React, { ReactNode } from 'react';

import { TopNavigation } from 'ui/common/components/layout/TopNavigation/TopNavigation';

import styles from 'ui/common/layouts/shared/layout.module.scss';

type Props = {
  children: ReactNode;
};

export const MemberLayout = ({ children }: Props) => {
  return (
    <main className={`xl:mx-auto min-h-screen ${styles.layoutMain}`}>
      <section className={styles.layoutSection}>
        <TopNavigation className={styles.topNavigation} />
        {children}
      </section>
    </main>
  );
};
