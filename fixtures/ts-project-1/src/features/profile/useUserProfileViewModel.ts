import { useState } from 'react';

export interface UserProfileState {
  displayName: string;
  email: string;
  isLoading: boolean;
}

export type UserProfileEvent =
  | { type: 'LOAD_PROFILE' }
  | { type: 'UPDATE_DISPLAY_NAME'; name: string };

export function useUserProfileViewModel() {
  const [state, setState] = useState<UserProfileState>({
    displayName: '',
    email: '',
    isLoading: false,
  });

  const onEvent = async (event: UserProfileEvent) => {
    switch (event.type) {
      case 'LOAD_PROFILE':
        setState(prev => ({ ...prev, isLoading: true }));
        break;
      case 'UPDATE_DISPLAY_NAME':
        setState(prev => ({ ...prev, displayName: event.name }));
        break;
    }
  };

  return { state, onEvent };
}
