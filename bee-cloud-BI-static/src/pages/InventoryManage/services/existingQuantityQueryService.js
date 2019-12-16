import { stringify } from 'querystring';
import request from '@/utils/request';
import { api_factory_prefix } from '@/constants/prefix';

/*
获取仓库树
*/
export async function getStorageTreeService(params) {
  return request(`${api_factory_prefix}/storage/getStorageTree?${stringify(params)}`);
}
/*
根据仓库id获取现存量
*/

export async function getStockByStorageIdService(params) {
  return request(`${api_factory_prefix}/storage/getStockByStorageId?${stringify(params)}`);
}
/*
根据仓库类型获取现存量
{
  currentPage:1,
  orderStage:'',
  pageSize:10,
  searchCount:true,
  storageType:'',
}
*/

export async function getStockByStorageTypeService(params) {
  return request(`${api_factory_prefix}/storage/getStockByStorageType?${stringify(params)}`);
}
