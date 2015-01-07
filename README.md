# Matrix
Ubiquitous VR in common lisp

Matrix is a Virtual Reality (VR) System. Virtual Reality is typically about fooling your senses into believing a false sense of reality (artificial reality). The current system aims at primarily fooling the eyes (maybe in the future it will extend to other things).

 The technical goal of this system is to be able an analog of a windowing system to VR environents. We want to provide application writers with the ability to easily write and deploy their applications semlessly in environents like the CAVE, Tiled-Walls, HMD (Occulus Rift) etc. We understand that not everybody has access display technologies and so the system will also serve as a desktop/tablet based windowing system as well.
 
 There are other projects like VRJuggler, VRUI, FreeVR, OmegaLib etc which at the outset seem like the current project. However this project is fundementally different due to its complete dynamic nature (due to it being written in Common Lisp) where application developers will have a design-time, build-time and debut-time all available at runtime. Which means there is a REPL, JIT and a TopLevel Debugger all available at the runtime (courtesy of Common-Lisp (CL)). The systems exploits data=code (homoiconic) semantics of CL making it possible for the code to be compiled and deployed at multiple sites simultaneously. Essentially this system allows for code migrating fault tolerant applications to be written.
 
