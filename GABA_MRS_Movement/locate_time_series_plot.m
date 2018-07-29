close all;
H = figure('Position',[1000,1000,900,150]);


for n = 1:length(P)
    clear mprage runs worksheet;
    load(P{n});
    
    for m = 1:length(runs)
        if range(runs(m).displacement.y)<1.5 || range(runs(m).displacement.y)>3,continue;end
        plot(runs(m).time,runs(m).displacement.y);
        title(runs(m).name);
        pause
    end
    for m = 1:length(mprages)
        if range(runs(m).displacement.y)<1.5 || range(runs(m).displacement.y)>3,continue;end
        plot(mprages(m).time,mprages(m).displacement.y);
        title(mprages(m).name);
        pause
    end
end

% VP093_101411 run1_v1
load('/hsgs/projects/jhyoon1/r21_gaba/reprocessing/results/postprocessing/VP093_101411_mprage_movement_postproc.mat');
close all;
H = figure('Position',[1000,1000,1500,300]);
N = 4; % run number
subplot(2,1,1);
plot(runs(N).time,runs(N).displacement.x,'r--');
hold on;
plot(runs(N).time,runs(N).displacement.y,'b');
axis([0 400 -4 2]);
ylabel('Displacement (mm)');
subplot(2,1,2);
plot(runs(N).time,runs(N).velocity.x*5,'r--');
hold on;
plot(runs(N).time,runs(N).velocity.y*5,'b');
axis([0 400 -4 2]);
legend('L-R','S-I');
xlabel('Time (s)');
ylabel('Velocity (mm/s)');