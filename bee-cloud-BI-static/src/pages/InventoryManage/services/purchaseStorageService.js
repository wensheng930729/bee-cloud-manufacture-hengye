import { stringify } from 'querystring';
import request from '@/utils/request';
import { api_factory_prefix } from '@/constants/prefix';

const prefix = `${api_factory_prefix}`;

// 待入库list
export async function purchaseStorageWaitService(params) {
  return request(`${prefix}/storage/selectBuyPendingStorageInfoByConditional?${stringify(params)}`);
  // return request(`${prefix}/storage/selectBuyPendingStorageInfoByConditional`, {
  //   method: 'POST',
  //   body: params,
  // });
}

// 已入库list
export async function purchaseStorageService(params) {
  return request(`${prefix}/storage/selectBuyStorageInfoByConditional?${stringify(params)}`);
  // return request(`${prefix}/storage/selectBuyStorageInfoByConditional`, {
  //   method: 'POST',
  //   body: params,
  // });
}

// 待入库-列表头部的  "新增入库"
/*
{
  "contractId": "string",
  "productId": 0,
  "productNumber": 0,
  "productSpecId": 0,
  "productSpecName": "string",
  "storageId": 0
}
*/
export async function saveFreeStorageService(params = {}) {
  return request(`${prefix}/storage/saveFreeStorage`, {
    method: 'POST',
    body: params,
  });
}

// 已入库-列表头部的  "查看新增的入库详情"
export async function getFreeStorageService(params) {
  return request(`${prefix}/storage/selectFreeInStorageRecord?${stringify(params)}`);
}

// 查看详情 {buyProductPendingStorageId}
export async function getDetailService(params) {
  return request(`${prefix}/storage/selectBuyStorageMsg?${stringify(params)}`);
}

// 详情界面- 提交按钮
/*
{
  "buyProductPendingStorageId": "string",
  "productId": 0,
  "productName": "string",
  "productNumber": 0,
  "productUnit": "string",
  "storageId": 0,
  "storageName": "string"
}
*/
export async function saveProductService(params = {}) {
  return request(`${prefix}/storage/saveProduct`, {
    method: 'POST',
    body: params,
  });
}
