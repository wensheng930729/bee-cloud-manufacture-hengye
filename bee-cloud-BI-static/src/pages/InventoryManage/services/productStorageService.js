import { stringify } from 'querystring';
import request from '@/utils/request';
import { api_factory_prefix } from '@/constants/prefix';

export async function selectFinishedStorageByConditional(params) {
  return request(`${api_factory_prefix}/storage/selectFinishedStorageByConditional?${stringify(params)}`);
}

export async function saveFinishedProduct(params) {
  return request(`${api_factory_prefix}/storage/saveFinishedProduct`, {
    method: 'POST',
    body: params,
  });
}
