import React, { ReactNode } from 'react';

import { SideNavigation } from 'ui/common/components/layout/SideNavigation/SideNavigation';
import { TopNavigation } from 'ui/common/components/layout/TopNavigation/TopNavigation';

import styles from 'ui/common/layouts/shared/layout.module.scss';

type Props = {
  children: ReactNode;
};

export const AdminLayout = ({ children }: Props) => {
  return (
    <main className={`xl:mx-auto min-h-screen ${styles.layoutMain}`}>
      <SideNavigation />
      <section className={styles.layoutSection}>
        <TopNavigation className={styles.topNavigation} />
        {children}
      </section>
    </main>
  );
};
