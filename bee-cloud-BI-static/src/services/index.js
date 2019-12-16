import { stringify } from 'querystring';
import apis from './api';
import request from '@/utils/request';
import { api_user_prefix,api_factory_prefix } from '@/constants/prefix';

export async function login(params) {
  return request(apis.login.api(), {
    method: apis.login.type,
    body: params,
  });
}

export async function logout(params) {
  return request(apis.logout.api(), {
    method: apis.logout.type,
    body: params,
  });
}

export async function getSelfInfo() {
  return request(apis.getSelfInfo.api(), {
    method: apis.getSelfInfo.type,
  });
}

export async function getSelfResource() {
  return request(apis.getSelfResource.api(), {
    method: apis.getSelfResource.type,
  });
}
export async function getProductListByCategory(params) {
  return request(apis.getProductListByCategory.api(params), {
    method: apis.getProductListByCategory.type,
  }).then(res => (res.code === 1 && res.object ? res.object : []));
}

export async function getDeviceListByType(params) {
  return request(apis.getDeviceListByType.api(), {
    method: apis.getDeviceListByType.type,
    body: params,
  }).then(res => (res.code === 1 && res.object ? res.object : []));
}

// 根据菜单类型获取tab栏目
export async function getEnableReportFormsList(params) {
  return request(`${api_factory_prefix}/configReportForms/getEnableReportFormsList?${stringify(params)}`);
}

// 获取物流订单
export async function getAllLogisticsContractList(params) {
  return request(`${api_factory_prefix}/buyLogisticsBatch/getAllLogisticsContractList?${stringify(params)}`);
}
// 获取采购订单
export async function buyContractBasic(params) {
  return request(`${api_factory_prefix}/buyContractBasic/getBuyContractList?${stringify(params)}`);
}
// 获取销售订单
export async function saleContractBasic(params) {
  return request(`${api_factory_prefix}/saleContractBasic/getBuyContractList?${stringify(params)}`);
}

/**
 * 根据产品id查询产品化验结果项
 * @param {*}
 * productId 产品id
 */
export async function getTestAttributeOutByProductId(productId) {
  return request(
    `${api_factory_prefix}/configProduct/getTestAttributeOutByProductId/${productId}`,
  );
}

// 获取产品列表
export async function getProducts(params) {
  return request(`${api_factory_prefix}/reportForm/getProducts?${stringify(params)}`);
}

// 获取所有供应商
export async function getSupplierListByCategory() {
  return request(`${api_user_prefix}/supplier/getSupplierListByCategory`, {
    method: 'POST',
    body: {
      type: [],
    },
  });
}

// 根据类别查询仓库列表
/*
rq:{
  types:[],
}
sysToken:''
*/
export async function getRepositoryListByType(params = {}) {
  return request(`${api_factory_prefix}/configRepository/getRepositoryListByType`, {
    method: 'POST',
    body: params,
  });
}

// 获取所有炉号
export async function getFurnaces(params) {
  return request(`${api_factory_prefix}/reportForm/getFurnaces?${stringify(params)}`);
}
// 获取班次
export async function listConfigCodeByType(params) {
  return request(`${api_factory_prefix}/configCode/listConfigCodeByType?${stringify(params)}`);
}

// 根据产品ID获取规格列表
export async function getProductSpecByProductId(productId) {
  return request(`${api_factory_prefix}/configProductSpec/getProductSpecByProductId/${productId}`);
}

//根据版本号查询升级描述
export async function getVersionMessage() {
  return request(apis.getVersionMessage.api(), {
    method: apis.getVersionMessage.type,
  });
}