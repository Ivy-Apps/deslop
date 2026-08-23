import { formatDate } from "../../../lib/util";

// PASSES no-react-transitively-in-data: only depends on @/lib/util which is React-free.
export const fetchHomeData = async () => {
  return {
    title: "Welcome Home",
    lastUpdated: formatDate(new Date()),
  };
};
