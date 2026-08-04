# Example Rulebooks

This directory contains production-ready example rulebooks for common TypeScript and React architectures.

> **These are examples, not a preset.** Each rulebook is independent — copy the ones that fit your project into your own `deslop/rules/` directory and adapt them. Rules across different files can conflict with each other by design (e.g. `mvi.yaml` and `feature-sliced-design.yaml` both restrict cross-feature imports but with different assumptions about your folder structure).

## Available Examples

| File | Architecture |
|------|-------------|
| [`global.yaml`](./global.yaml) | Universal rules that apply to any TypeScript codebase |
| [`mvi.yaml`](./mvi.yaml) | Model-View-Intent — Containers, Views, ViewModels |
| [`clean-architecture.yaml`](./clean-architecture.yaml) | Uncle Bob's Clean Architecture — domain/application/infrastructure/presentation layers |
| [`feature-sliced-design.yaml`](./feature-sliced-design.yaml) | Feature Sliced Design — strict layer hierarchy |
| [`nextjs-app-router.yaml`](./nextjs-app-router.yaml) | Next.js App Router — server/client boundary, route handlers, server actions |
| [`quality.yaml`](./quality.yaml) | Quality standards — test coverage and Storybook requirements |
