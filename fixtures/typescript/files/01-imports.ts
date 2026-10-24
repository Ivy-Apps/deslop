import { formatDate } from '../../lib/utils';
import * as Logger from '../../lib/logger';
import User from '../models/User';

function main() {
  Logger.log(formatDate(new Date()));
  const u = new User();
}