import { HomeScreen } from "../../home/home-screen";

// VIOLATION of no-react-transitively-in-data: importing a React component
// creates a transitive dependency on React in the data layer.
// Chain: profile-repository → home-screen → react
export const getProfileWidget = () => {
  return { component: HomeScreen };
};
