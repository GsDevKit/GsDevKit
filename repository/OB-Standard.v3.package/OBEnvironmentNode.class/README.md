OBEnvironmentNode wraps an instance of SystemDictionary. In current Squeak images, there is only one such instance, but OB-Standard is coded so as to use rely on this assumption as little as possible. Thus OBEnvironmentNode typically serves as the root of the standard browser, and passes its environment on to other nodes in the tree.