import csv
import hashlib
import json
import unicodedata
import zipfile
from collections import Counter, defaultdict
from pathlib import Path

root = Path('work/vote_audit')
zip_path = root / 'kv-2017_ehd_maa.csv.zip'
saved = list(csv.DictReader((root / 'saved_candidates_readonly.csv').open(encoding='utf-8')))
def norm(s):
    return unicodedata.normalize('NFC', s).strip().casefold()
def norm_name(s):
    return ' '.join(norm(s).replace('""','"').split())

fields = ['city_code','city','candidate_number','firstname','lastname','party',
          'gender_code','age','occupation','member_european_parliament',
          'member_national_parliament','incumbent_councillor','advance_votes',
          'election_day_votes','official_votes','elected_code','comparison_number',
          'rank','final_rank','count_status','count_phase','last_updated']
indices = [2,15,14,17,18,11,19,20,21,26,27,28,32,33,34,38,39,40,41,42,43,44]
all_municipal = []
stats = Counter()
digest = hashlib.md5()
with zipfile.ZipFile(zip_path) as z:
    archive_names = z.namelist()
    assert len(archive_names) == 1
    with z.open(archive_names[0]) as source:
        for raw in source:
            digest.update(raw)
            stats['all_records'] += 1
            row = [s.strip() for s in raw.decode('iso-8859-1').rstrip('\r\n').split(';')]
            stats['record_type_'+row[3]] += 1
            if row[3] != 'K':
                continue
            assert row[4] == '****'
            assert row[0] == 'K'
            obs = {name: row[idx] for name,idx in zip(fields,indices)}
            all_municipal.append(obs)

published_md5 = (root / 'kv-2017_ehd_maa.csv.chk').read_text().strip()
observed_md5 = digest.hexdigest()
assert observed_md5 == published_md5, (observed_md5,published_md5)
official = defaultdict(list)
official_name = defaultdict(list)
city_votes=Counter()
for obs in all_municipal:
    official[(norm(obs['city']), int(obs['candidate_number']))].append(obs)
    official_name[(norm(obs['city']),norm(obs['party']),norm_name(obs['firstname']),norm_name(obs['lastname']))].append(obs)
    city_votes[obs['city_code']]+=int(obs['official_votes'])
duplicates = {str(k): len(v) for k,v in official.items() if len(v)>1}
assert not duplicates, duplicates

audit = []
for obs in saved:
    key=(norm(obs['city']),int(obs['candidate_number']))
    matched=official.get(key,[])
    out=dict(obs)
    out['official_matches']=len(matched)
    if len(matched)==1:
        off=matched[0]
        out.update({'official_'+k:v for k,v in off.items() if k!='official_votes'})
        out['official_votes']=off['official_votes']
        out['firstname_exact']=norm(obs['firstname'])==norm(off['firstname'])
        out['lastname_exact']=norm(obs['lastname'])==norm(off['lastname'])
        count=int(obs['turnout']); ref=int(off['official_votes'])
        out['saved_to_official_ratio']=count/ref if ref else None
        out['saved_equals_official']=count==ref
        out['saved_equals_double_official']=count==2*ref
        out['saved_minus_double_official']=count-2*ref
        out['saved_minus_official']=count-ref
        out['gender_matches_number'] = None if obs['gender']=='NA' else ({'Male':'1','Female':'2'}[obs['gender']]==off['gender_code'])
        out['age_matches_number'] = None if obs['age']=='NA' else (float(obs['age'])==float(off['age']))
        out['municipality_total_valid_votes']=city_votes[off['city_code']]
        out['official_vote_share']=ref/city_votes[off['city_code']]
        nk=(norm(obs['city']),norm(obs['party']),norm_name(obs['firstname']),norm_name(obs['lastname']))
        name_matches=official_name.get(nk,[])
        out['full_name_official_matches']=len(name_matches)
        if len(name_matches)==1:
            name_off=name_matches[0]
            out['official_number_for_saved_full_name']=name_off['candidate_number']
            out['official_votes_for_saved_full_name']=int(name_off['official_votes'])
            out['saved_count_double_name_votes']=count==2*int(name_off['official_votes'])
            out['source_gender_matches_saved_name_person']=None if obs['gender']=='NA' else ({'Male':'1','Female':'2'}[obs['gender']]==name_off['gender_code'])
            out['source_age_matches_saved_name_person']=None if obs['age']=='NA' else (float(obs['age'])==float(name_off['age']))
            out['same_number_person_as_saved_name_person']=int(off['candidate_number'])==int(name_off['candidate_number'])
    audit.append(out)

def write_csv(path, rows, columns=None):
    if columns is None: columns=list(dict.fromkeys(k for row in rows for k in row))
    with path.open('w',encoding='utf-8',newline='') as f:
        writer=csv.DictWriter(f,fieldnames=columns)
        writer.writeheader();writer.writerows(rows)
write_csv(root/'official_municipal_candidate_returns.csv',all_municipal,fields)
write_csv(root/'vote_count_audit_all_saved_candidates.csv',audit)
unmatched=[o for o in audit if o['official_matches']!=1]
count_mismatches=[o for o in audit if o['official_matches']==1 and not o['saved_equals_double_official']]
name_mismatches=[o for o in audit if o['official_matches']==1 and not(o['firstname_exact'] and o['lastname_exact'])]
write_csv(root/'unmatched_candidates.csv',unmatched,list(audit[0]))
write_csv(root/'not_double_official.csv',count_mismatches,list(audit[0]))
write_csv(root/'name_mismatches.csv',name_mismatches,list(audit[0]))
summary={
    'source_url':'https://tulospalvelu.vaalit.fi/KV-2017/kv-2017_ehd_maa.csv.zip',
    'dictionary_url':'https://tulospalvelu.vaalit.fi/KV-2017/ohje/KV2017_CSV-tiedostojen_kuvaus_fi.pdf',
    'md5_uncompressed_observed':observed_md5,'md5_published':published_md5,
    'checksum_matches':observed_md5==published_md5,
    'archive_member':archive_names[0],
    'archive_zip_md5':hashlib.md5(zip_path.read_bytes()).hexdigest(),
    'archive_records':dict(stats),'official_municipal_rows':len(all_municipal),
    'official_unique_city_candidate_keys':len(official),'duplicate_official_keys':len(duplicates),
    'saved_rows':len(saved),'saved_unique_cities':len({o['city'] for o in saved}),
    'unique_official_matches':sum(o['official_matches']==1 for o in audit),
    'unmatched_saved_rows':len(unmatched),
    'saved_equals_official':sum(o.get('saved_equals_official',False) for o in audit),
    'saved_equals_double_official':sum(o.get('saved_equals_double_official',False) for o in audit),
    'not_double_official':len(count_mismatches),
    'firstname_exact':sum(o.get('firstname_exact',False) for o in audit),
    'lastname_exact':sum(o.get('lastname_exact',False) for o in audit),
    'name_mismatch_rows':len(name_mismatches),
    'matched_saved_elected_codes':dict(Counter(o.get('official_elected_code','missing') for o in audit)),
    'official_count_phase':dict(Counter(o['count_phase'] for o in all_municipal)),
    'official_count_status':dict(Counter(o['count_status'] for o in all_municipal)),
    'official_city_count':len({o['city'] for o in all_municipal}),
    'official_city_code_count':len({o['city_code'] for o in all_municipal}),
    'source_gender_observed':sum(o.get('gender_matches_number') is not None for o in audit),
    'source_gender_matches_official_number':sum(o.get('gender_matches_number') is True for o in audit),
    'source_age_observed':sum(o.get('age_matches_number') is not None for o in audit),
    'source_age_matches_official_number':sum(o.get('age_matches_number') is True for o in audit),
    'saved_full_name_unique_official_matches':sum(o.get('full_name_official_matches')==1 for o in audit),
    'saved_counts_double_votes_of_saved_full_name':sum(o.get('saved_count_double_name_votes') is True for o in audit),
    'different_person_numbers_given_full_name':sum(o.get('same_number_person_as_saved_name_person') is False for o in audit),
    'sum_official_votes_all_candidates':sum(city_votes.values()),
}
(root/'audit_summary.json').write_text(json.dumps(summary,indent=2,ensure_ascii=False),encoding='utf-8')
print(json.dumps(summary,indent=2,ensure_ascii=False))
print('Count mismatches:')
for o in count_mismatches: print({k:o.get(k) for k in ['id','city','candidate_number','firstname','lastname','turnout','official_votes','saved_to_official_ratio']})
print('Name mismatches:')
for o in name_mismatches: print({k:o.get(k) for k in ['id','city','candidate_number','firstname','official_firstname','lastname','official_lastname','turnout','official_votes']})
