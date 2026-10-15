(function(){'use strict';
const $=id=>document.getElementById(id);
const MODE_HELP={
  off:'Nothing is computed and nothing is added to the agent\'s context.',
  shadow:'The state is tracked and each turn records what would have been added, but the agent is given nothing extra.',
  tone:'A short fixed sentence is added to the agent\'s context when the state is not baseline.'
};
const UNIX_OFFSET=2208988800;
// Stored-state problems the operator can clear with a reset. "uninitialized" and
// "no-stores" are not listed: the first fixes itself on the next turn, the second is not
// fixable from here.
const RECOVERABLE=['incompatible-policy','corrupt-state','source-binding-mismatch','checkpoint-ahead'];
let busy=false;
function show(text,kind){const s=$('status');s.textContent=text;s.className='status'+(kind?' '+kind:'')}
async function request(path,options={}){
  const response=await fetch(path,{cache:'no-store',credentials:'same-origin',...options,
    headers:{'X-PAI-Request':'same-origin',...(options.headers||{})}});
  const text=await response.text();let data;
  try{data=JSON.parse(text)}catch(_){throw new Error('HTTP '+response.status+': invalid JSON')}
  if(!response.ok)throw new Error(data.error||'HTTP '+response.status);
  return data;
}
function post(path,body){
  return request(path,{method:'POST',headers:{'Content-Type':'application/json'},body:JSON.stringify(body||{})});
}
function when(universal){
  if(typeof universal!=='number')return 'unknown time';
  const d=new Date((universal-UNIX_OFFSET)*1000);
  return Number.isNaN(d.getTime())?'unknown time':d.toLocaleString();
}
function add(list,text){const li=document.createElement('li');li.textContent=text;list.appendChild(li);return li}
function kv(dl,key,value){
  const dt=document.createElement('dt'),dd=document.createElement('dd');
  dt.textContent=key;dd.textContent=value;dl.append(dt,dd);
}
function renderMode(policy,report){
  const mode=policy.affect_expression||'off';
  for(const m of ['off','shadow','tone'])$('mode-'+m).setAttribute('aria-pressed',String(m===mode));
  $('mode-help').textContent=MODE_HELP[mode]||'';
  $('max-level').value=policy.modulation_max_level||'repeating';
  const paused=policy.pause_all_affective===true;
  $('pause-toggle').textContent=paused?'Resume affect':'Pause all affect';
  $('pause-toggle').setAttribute('aria-pressed',String(paused));
  $('disable-now').disabled=(mode==='off');
  let text='Mode: '+(mode==='tone'?'Active':mode.charAt(0).toUpperCase()+mode.slice(1));
  if(paused)text+=' · PAUSED (nothing is applied)';
  return {text,paused};
}
function renderState(report){
  const dl=$('state');dl.replaceChildren();
  const proj=report.projection||{};
  kv(dl,'State source',report.source||'unknown');
  if(proj.status==='available'){
    kv(dl,'Level',proj.level);
    kv(dl,'Band',proj.band);
    kv(dl,'Strain figure',String(proj.coping_milliunits)+' of 500 (lower is more strained)');
    kv(dl,'Last setback',proj.last_setback_at?when(proj.last_setback_at):'none recorded');
    kv(dl,'Read through event',String(proj.through_event_id));
    const debts=(proj.debts||[]).map(d=>d.tool+' '+d.debt_milliunits).join(', ');
    kv(dl,'Tools with strain',debts||'none');
    const rep=(proj.repeating_threads||[]).map(r=>r.tool+' ×'+r.consecutive+' in '+r.thread_id).join('; ');
    kv(dl,'Repeating',rep||'none');
  }else if(proj.status==='inactive'){
    kv(dl,'Level','not tracked while Off');
  }else{
    kv(dl,'Level','unavailable');
    kv(dl,'Reason',proj.reason||'unknown');
  }
  const recoverable=proj.status==='unavailable'&&RECOVERABLE.includes(proj.reason);
  $('recovery').hidden=!recoverable;
  if(recoverable)$('recovery-reason').textContent=proj.reason;
  const list=$('evidence');list.replaceChildren();
  const evidence=proj.evidence||[];
  if(!evidence.length)add(list,'No evidence yet.');
  for(const e of evidence)add(list,'event '+e.event_id+' · '+e.effect);
}
function renderAudiences(report){
  const root=$('audiences');root.replaceChildren();
  for(const a of report.audiences||[]){
    const box=document.createElement('div');box.style.marginBottom='10px';
    const head=document.createElement('div');
    head.textContent=(a.audience==='private'?'Private deliberation':'Operator-facing replies')+' · state '+a.state+(a.label_id&&a.label_id!==null?' · '+a.label_id:'');
    head.className=a.applied?'good':(a.would_offer?'warn':'hint');
    box.appendChild(head);
    const note=document.createElement('div');note.className='hint';
    note.textContent=a.applied?'Applied now:':(a.would_offer?'Would be applied in Active mode:':'Nothing is added.');
    box.appendChild(note);
    if(a.text){const s=document.createElement('div');s.className='sentence';s.textContent=a.text;box.appendChild(s)}
    root.appendChild(box);
  }
}
function renderRecent(report){
  const list=$('recent');list.replaceChildren();
  const rows=report.recent_influence||[];
  if(!rows.length)add(list,'None yet.');
  for(const r of rows)add(list,when(r.at)+' · '+(r.status==='tone-injected'?'applied':'shadow')+' · '+r.audience+' · '+r.state+' · '+r.label_id+(r.event_id!==null&&r.event_id!==undefined?' · turn event '+r.event_id:''));
}
async function load(message){
  try{
    const [policyResponse,report]=await Promise.all([request('/api/relationship/policy'),request('/api/relationship/tone')]);
    const policy=policyResponse.policy||{};
    const mode=renderMode(policy,report);
    renderState(report);renderAudiences(report);renderRecent(report);
    show(message||mode.text,mode.paused?'warn':'');
  }catch(error){show(error.message,'error')}
}
async function act(label,work){
  if(busy)return;busy=true;show(label+'…');
  try{await work();await load(label+' done.')}
  catch(error){
    // A failed change must not leave a control showing a value the server does not hold:
    // re-read the real state first, then say what went wrong.
    const message=error.message;await load();show(message,'error');
  }finally{busy=false}
}
for(const mode of ['off','shadow','tone'])
  $('mode-'+mode).onclick=()=>act('Setting '+mode,()=>post('/api/relationship/policy',{affect_expression:mode}));
$('disable-now').onclick=()=>act('Disabling',()=>post('/api/relationship/policy',{affect_expression:'off'}));
// Decide pause versus resume from the server's current state, not from this page's copy,
// which may be up to 15 seconds old or changed from another device.
$('pause-toggle').onclick=()=>act('Updating pause',async()=>{
  const current=(await request('/api/relationship/policy')).policy||{};
  return post(current.pause_all_affective===true?'/api/relationship/resume':'/api/relationship/pause');
});
$('reset-state').onclick=()=>{
  if(!confirm('Reset the affect state? Recorded strain is forgotten and tracking restarts from now.'))return;
  act('Resetting',()=>post('/api/relationship/affect/reset'));
};
$('max-level').onchange=()=>act('Saving strength',()=>post('/api/relationship/policy',{modulation_max_level:$('max-level').value}));
$('refresh').onclick=()=>load();
$('instance').textContent='Instance: '+location.host;
load();
setInterval(()=>{if(!document.hidden&&!busy)load()},15000);
})();
