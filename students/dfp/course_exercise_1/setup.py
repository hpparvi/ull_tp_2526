import random

n = 10
dt = 0.001
dt_out = 0.1
t_end = 100.

with open("setup.txt", "w") as f:
    f.write(f"{dt}         ! dt (time step)\n")
    f.write(f"{dt_out}      ! dt_out (output interval)\n")
    f.write(f"{t_end}       ! t_end (total simulation time)\n")
    f.write(f"{n}          ! n (number of particles)\n")
    for i in range(n):
        m = 1.
        x = random.uniform(-1., 1.)
        y = random.uniform(-1., 1.)
        z = random.uniform(-1., 1.)
        vx = random.uniform(-0.5, 0.5)
        vy = random.uniform(-0.5, 0.5)
        vz = random.uniform(-0.5, 0.5)
        f.write(f"{m:.16e} {x:.16e} {y:.16e} {z:.16e} {vx:.16e} {vy:.16e} {vz:.16e}\n")