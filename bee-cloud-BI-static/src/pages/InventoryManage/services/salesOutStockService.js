import { stringify } from 'querystring';
import request from '@/utils/request';
import { api_factory_prefix } from '@/constants/prefix';

const prefix = `${api_factory_prefix}`;

// 待出库list
export async function selectUnLoadContractCar(params) {
  // return request(`${prefix}/storage/selectUnLoadContractCar?${stringify(params)}`);
  return request(`${prefix}/storage/webSearchSaleToBeOutOfStockList`, {
    method: 'POST',
    body: params,
  });
}

// 已出库list
export async function searchSaleOutOfStockList(params) {
  // return request(`${prefix}/storage/searchSaleOutOfStockList?${stringify(params)}`);
  return request(`${prefix}/storage/searchSaleOutOfStockList`, {
    method: 'POST',
    body: params,
  });
}

// 待入库-列表头部的  "新增入库"
/*

*/
export async function pickOutProduct(params = {}) {
  return request(`${prefix}/storage/pickOutProduct`, {
    method: 'POST',
    body: params,
  });
}

// 已出库-列表头部的  "查看新增的出库详情"
export async function getSaleNewOutOfStockDetails(params) {
  return request(`${prefix}/storage/getSaleNewOutOfStockDetails?${stringify(params)}`);
}

// 待出库 已出库 -查看详情
export async function getSaleToBeDeliveredById(params) {
  const { id, isOutStock } = params;
  return String(isOutStock) === 'false'
    ? request(`${prefix}/storage/getSaleToBeDeliveredById/${id}`)
    : request(`${prefix}/storage/getSaleOutOfStockById/${id}`);
}

// 详情界面- 提交按钮
/*

*/
export async function bulkOutTonBag(params = {}) {
  return request(`${prefix}/storage/bulkOutTonBag`, {
    method: 'POST',
    body: params,
  });
}
