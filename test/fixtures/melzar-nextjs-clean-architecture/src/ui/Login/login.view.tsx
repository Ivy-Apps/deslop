import React from 'react';

import { ContainerBox } from 'ui/common/components/layout/ContainerBox/ContainerBox';
import { TextLogo } from 'ui/common/components/typography/TextLogo/TextLogo';
import { LoginForm } from 'ui/Login/components/LoginForm/loginForm';

import styles from 'ui/Login/login.module.scss';

export const LoginView = () => (
  <main
    data-testid="loginScreen"
    className={`xl:container xl:mx-auto min-h-screen p-24 ${styles.container}`}
  >
    <ContainerBox className={styles.loginContainer}>
      <TextLogo className={styles.logo}>Clean</TextLogo>
      <LoginForm />
    </ContainerBox>
  </main>
);
