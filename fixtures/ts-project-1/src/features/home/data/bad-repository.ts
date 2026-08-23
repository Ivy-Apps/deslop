import { HomeScreen } from "../home-screen";

// VIOLATION of no-react-transitively-in-data: importing a React component from
// the data layer creates a transitive dependency on React.
// Chain: bad-repository → home-screen → react
export const getBadData = () => {
  return { component: HomeScreen };
};
