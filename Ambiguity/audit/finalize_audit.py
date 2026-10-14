import csv,json
from collections import Counter
from pathlib import Path
root=Path('work/vote_audit')
audit=list(csv.DictReader((root/'vote_count_audit_all_saved_candidates.csv').open(encoding='utf-8')))
out=[]
for r in audit:
    o={
      'id':int(r['id']), 'city':r['city'], 'candidate_number':int(r['candidate_number']),
      'official_city_code':r['official_city_code'],
      'official_votes':int(r['official_votes']),
      'city_valid_votes':int(r['municipality_total_valid_votes']),
      'official_vote_share':float(r['official_vote_share']),
      'elected':int(r['official_elected_code']=='1'),
      'elected_status_code':int(r['official_elected_code']),
      'elected_status':{'1':'elected','2':'alternate','3':'not elected'}[r['official_elected_code']],
      'official_firstname':r['official_firstname'],'official_lastname':r['official_lastname'],
      'official_party':r['official_party'],'official_gender':{'1':'Male','2':'Female'}[r['official_gender_code']],
      'official_age':int(r['official_age']),
      'official_incumbent_councillor':int(r['official_incumbent_councillor']=='1'),
      'official_member_national_parliament':int(r['official_member_national_parliament']=='1'),
      'official_member_european_parliament':int(r['official_member_european_parliament']=='1'),
      'saved_firstname':r['firstname'],'saved_votes':int(r['turnout']),
      'saved_name_belongs_other_candidate':r['same_number_person_as_saved_name_person']=='False',
      'saved_votes_halved_differ_from_photo_candidate':not(r['saved_equals_double_official']=='True'),
      'metadata_city_candidate_verified':True,
      'appearance_photo_id_verified':True,
      'appearance_age_verified':r['age_matches_number']=='True' if r['age_matches_number'] else None,
      'appearance_gender_verified':r['gender_matches_number']=='True' if r['gender_matches_number'] else None,
      'official_count_status':r['official_count_status'],
      'official_count_phase':r['official_count_phase'],
      'official_last_updated':r['official_last_updated'],
    }
    assert o['official_votes'] >= 0 and o['city_valid_votes'] > 0
    out.append(o)
assert len(out)==1389 and len({r['id'] for r in out})==1389
with (root/'verified_photo_candidate_crosswalk.csv').open('w',encoding='utf-8',newline='') as f:
    w=csv.DictWriter(f,fieldnames=list(out[0]));w.writeheader();w.writerows(out)
city_rows={}
official=list(csv.DictReader((root/'official_municipal_candidate_returns.csv').open(encoding='utf-8')))
for r in official:
    code=r['city_code']
    if code not in city_rows: city_rows[code]={'city_code':code,'city':r['city'],'valid_votes':0,'candidates':0,'elected_councillors':0}
    city_rows[code]['valid_votes']+=int(r['official_votes'])
    city_rows[code]['candidates']+=1
    city_rows[code]['elected_councillors']+=int(r['elected_code']=='1')
with (root/'official_municipality_vote_totals.csv').open('w',encoding='utf-8',newline='') as f:
    w=csv.DictWriter(f,fieldnames=list(next(iter(city_rows.values()))));w.writeheader();w.writerows(sorted(city_rows.values(),key=lambda z:z['city_code']))
print('Crosswalk',len(out),'photo candidates; zero votes',sum(r['official_votes']==0 for r in out))
print('Different-name people',sum(r['saved_name_belongs_other_candidate'] for r in out))
print('Halved source count still wrong',sum(r['saved_votes_halved_differ_from_photo_candidate'] for r in out))
print('Cities',len(city_rows),'valid votes',sum(r['valid_votes'] for r in city_rows.values()),'elected',sum(r['elected_councillors'] for r in city_rows.values()))
