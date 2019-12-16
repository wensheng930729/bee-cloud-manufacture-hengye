import { stringify } from 'querystring';
import request from '@/utils/request';
import { api_factory_prefix } from '@/constants/prefix';

const prefix = `${api_factory_prefix}/stockInventory`;

//盘点单列表查询
export async function getList(params) {
  return request(`${prefix}/inventoryInfo/list?${stringify(params)}`);
}

//根据盘点单号查询盘点单详细
export async function getDetail(params) {
  return request(`${prefix}/inventoryInfo/${params}`);
}

// 新增盘点单
export async function createService(params) {
  return request(`${prefix}/create`, {
    method: 'POST',
    body: params,
  });
}

// 获取盘点单类型
/*
    {
      "inventoryTypeCode": 0,
      "inventoryTypeDesc": "string"
    }
*/
export async function getInventoryTypeService(params) {
  return request(`${prefix}/inventory/type`);
}
/*
{inventoryTypeCode}  根据盘点分类获得下拉列表详细
*/
export async function getinventoryTypeDescService(params) {
  return request(`${prefix}/inventoryType/desc?${stringify(params)}`);
}

// 创建库存盘点单，并返回待盘点数据
export async function createInventoryOrder(params) {
  return request(`${prefix}/create/inventoryOrder`, {
    method: 'POST',
    body: params,
  });
}

// 保存盘点单
export async function saveInventoryInfo(params) {
  return request(`${prefix}/save/inventoryInfo`, {
    method: 'POST',
    body: params,
  });
}


