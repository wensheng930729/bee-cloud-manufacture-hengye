import React, { Component } from 'react';
import { Row, Col, Card, message } from 'antd';
import { connect } from 'dva';
import CustomTree from '@/components/FormWidget/FormItemTree/CustomTree';
import { StandardTable } from '@/components/FormWidget';

import styles from './index.less';

const columns = [
  {
    title: '产品名称',
    dataIndex: 'productName',
  },
  {
    title: '规格',
    dataIndex: 'productSpecName',
  },
  {
    title: '数量/单位',
    dataIndex: 'productNumber',
    render: (text, record) => `${record.productNumber}${record.productUnit}`,
  },
  {
    title: '存放库位',
    dataIndex: 'storageName',
  },
  {
    title: '更新时间',
    dataIndex: 'modifyTime',
  },
];

@connect(({ existingQuantityQueryModel, loading }) => ({
  storageTreeData: existingQuantityQueryModel.storageTreeData,
  stockData: existingQuantityQueryModel.stockData,
  loading:
    loading.effects['existingQuantityQueryModel/getStockByStorageIdEffect'] ||
    loading.effects['existingQuantityQueryModel/getStockByStorageTypeEffect'],
}))
class Index extends Component {
  constructor(props) {
    super(props);
    this.state = {
      searchParams: {
        currentPage: 1,
        orderStage: '',
        pageSize: 10,
        searchCount: true,
        storageType: '',
        storageId: '',
      },
    };
  }

  componentDidMount() {
    const { dispatch } = this.props;
    dispatch({
      type: 'existingQuantityQueryModel/getStorageTreeEffect',
      payload: {},
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  }

  handleTreeSelect = key => {
    if (!(key && key[0])) {
      return;
    }
    const keyStr = key[0];

    let newsearchParams = {
      currentPage: 1,
      orderStage: '',
      pageSize: 10,
      searchCount: true,
    };

    if (!keyStr.includes('undefined') && keyStr.includes('-')) {
      newsearchParams = { ...newsearchParams, storageId: keyStr.split('-')[1], storageType: null };
      this.getListById(newsearchParams);
    } else {
      newsearchParams = {
        ...newsearchParams,
        storageType: keyStr !== 'all' ? keyStr : '',
        storageId: null,
      };
      this.getListByType(newsearchParams);
    }
    this.setState({
      searchParams: { ...newsearchParams },
    });
  };

  getListById = payload => {
    const { dispatch } = this.props;
    dispatch({
      type: 'existingQuantityQueryModel/getStockByStorageIdEffect',
      payload,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  };

  getListByType = payload => {
    const { dispatch } = this.props;
    dispatch({
      type: 'existingQuantityQueryModel/getStockByStorageTypeEffect',
      payload,
      callback: (data, status, msg) => {
        if (status === 'error') {
          message.error(msg);
        }
      },
    });
  };

  handleTableChange = pagination => {
    const { current: currentPage, pageSize } = pagination;
    const { searchParams } = this.state;
    const newSearchParams = { ...searchParams, currentPage, pageSize };
    if (newSearchParams.storageId) {
      this.getListById(newSearchParams);
    } else if (
      newSearchParams.storageType ||
      !(newSearchParams.storageType && newSearchParams.storageId)
    ) {
      this.getListByType(newSearchParams);
    }
  };

  render() {
    const { storageTreeData, stockData, loading } = this.props;
    const tableProps = {
      loading,
      columns,
      data: stockData,
      bordered: true,
      onChange: this.handleTableChange,
      //
    };

    return (
      <Card>
        <Row gutter={16}>
          <Col span={5} className={styles.treeContainerStyle}>
            <h3>现存量查询</h3>
            <CustomTree treeData={storageTreeData} onSelect={this.handleTreeSelect} />
          </Col>
          <Col span={16}>
            <StandardTable {...tableProps} />
          </Col>
        </Row>
      </Card>
    );
  }
}

export default Index;
