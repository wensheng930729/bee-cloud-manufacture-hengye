import { stringify } from 'querystring';
import request from '@/utils/request';
import { api_factory_prefix } from '@/constants/prefix';

export async function getWeighListInfo(params) {
  return request(`${api_factory_prefix}/weightMachine/getWeighListInfo?${stringify(params)}`);
}
