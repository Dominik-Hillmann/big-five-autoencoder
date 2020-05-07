from abc import ABC, abstractmethod

class Model(ABC):
    """Base class for all models."""

    def __call__(self, batch_X):
        return self.pred(batch_X)


    def pred(self, batch_X):
        