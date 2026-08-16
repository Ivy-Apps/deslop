import {
  After,
  AfterAll,
  Before,
  BeforeAll,
  setDefaultTimeout,
} from '@cucumber/cucumber';
import { chromium, ChromiumBrowser } from 'playwright-core';

let browser: ChromiumBrowser;
setDefaultTimeout(60 * 1000);
BeforeAll(async function beforeAll() {
  browser = await chromium.launch({ headless: true });
});

Before(async function before() {
  this.context = await browser.newContext();
  this.page = await this.context.newPage();
});

After(async function after() {
  this.context.page?.close();
  this.context.context?.close();
});

AfterAll(async function afterAll() {
  await browser.close();
});
