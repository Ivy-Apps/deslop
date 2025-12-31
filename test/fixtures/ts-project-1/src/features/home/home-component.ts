import { formatDate } from "../../lib/util";

export const getHomeData = () => {
  return {
    title: "Welcome Home",
    lastUpdated: formatDate(new Date()),
  };
};