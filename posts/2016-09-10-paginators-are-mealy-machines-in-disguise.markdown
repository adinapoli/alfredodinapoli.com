---
title: Paginators are Mealy Machines in disguise
tags: haskell, fp
---

# Paginators are Mealy Machines in disguise

```
Summary: At work I needed to stream some data out from a service which returned
data in paginated chunks. Using a very simple data type based on Mealy Machines
worked surprisingly well.
```

One of the aspect I enjoy most of programming is when you had the chance of
applying something you have learned to the real world. A couple of weeks ago
I needed to create a tool to "garbage collect" old [ECR]() images. Very simply
put, _ECR_ stands for _Elastic Container Registry_ and is no more, no less,
a private Docker Registry you can use as part of the impressive AWS (_Amazon Web Service_) toolkit.

_ECR_ works by having a set of _repositories_ and for each of them you can upload up to 500 Docker images,
unless you request to extend this upper bound. In all honesty, 500 Docker images sounds quite a lot (and
they are!), so our team thought was a good hygiene practice to delete stale images 
