import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import imageio.v2 as imageio
from pathlib import Path

L = 100
B = 100
N = 150
iterations = 500
tick = 0.1

fig, (ax1, ax2) = plt.subplots(1, 2, figsize=(16, 8))

for i in range(1,iterations+1):
    
    print(f"now at iteration {i}",flush=True),
    
    if i < 10:
        read_name = f'../data/particle_status00{i}.dat'
        save_location = f'../images/status00{i}.png'
    elif i < 100:
        read_name = f'../data/particle_status0{i}.dat'
        save_location = f'../images/status0{i}.png'
    else:
        read_name = f'../data/particle_status{i}.dat'
        save_location = f'../images/status{i}.png'

    data = pd.read_csv(read_name)
    
    x = data.iloc[:,0]
    y = data.iloc[:,1]
    
    vx = data.iloc[:,2]
    vy = data.iloc[:,3]

    net_velocity = np.sqrt(vx**2 + vy**2)
        
    ax1.clear()
    ax1.scatter(x,y,color="black")
    
    ax1.set_xlim(0,L)
    ax1.set_ylim(0,B)
    ax1.set_xticks([])
    ax1.set_yticks([])
    
    ax1.set_xlabel(f"Number of entities = {N}")
    ax1.set_title(f"time step = {(i * tick):.2f}")

    ax2.clear()

    n , bins , patches = ax2.hist(net_velocity , bins = 50 , density = True)
    
    for patch, height in zip(patches, n):
        color = plt.cm.viridis(height / max(n))
        patch.set_facecolor(color)
    
    ax2.set_ylabel("Probability_distribution")
    ax2.set_title(f"Maxwell-Boltzmann Distribution:")
    
    plt.savefig(save_location)
   
print("Files written...",flush=True)

folder_path = Path('../images/')
images = list(folder_path.glob('status*.png'))
images.sort()

image_list = [imageio.imread(str(img)) for img in images]

output_path = '../images/animation.mov'
imageio.mimsave(output_path, image_list, fps=10)

print("Animation complete!")