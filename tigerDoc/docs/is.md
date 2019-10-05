---
id: is 
title: Instruction Selection
sidebar_label: Instruction Selection
header-includes: \usepackage{minted}
---

Now that we have done Canonisation, our compiler will now call code generator. It will basically convert the body into assembly language code with the restriction that we will still be using temporaries instead of actual machine registers. In this form, source registers will be labelled `` `si`` and destination registers as `` `di``.